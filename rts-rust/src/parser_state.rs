use crate as ddl;
use ddl::Clo;
use serde::Serialize;
use std::collections::VecDeque;
use std::fmt;

// ============================================================================
// Parser Context Tracking
// ============================================================================

const MAX_TAIL_CALL_RUNS: usize = 64;

#[derive(Clone)]
pub struct TailCallRun {
    label: String,
    count: usize,
}

/// A frame in the parser context stack.
#[derive(Clone)]
pub enum ParserContextFrame {
    Call {
        label: String,
    },
    TailCalls {
        entry: String,
        recent: VecDeque<TailCallRun>,
        omitted: usize,
    },
}

impl ParserContextFrame {
    pub fn new(label: String) -> Self {
        ParserContextFrame::Call { label }
    }

    pub fn tail_call(&mut self, label: String) {
        match self {
            ParserContextFrame::Call { label: call_site } => {
                let entry = std::mem::take(call_site);
                let mut recent = VecDeque::new();
                recent.push_back(TailCallRun {
                    label,
                    count: 1,
                });
                *self = ParserContextFrame::TailCalls {
                    entry,
                    recent,
                    omitted: 0,
                };
            }
            ParserContextFrame::TailCalls {
                recent, omitted, ..
            } => {
                if let Some(run) = recent.back_mut() {
                    if run.label == label {
                        run.count += 1;
                        return;
                    }
                }

                if recent.len() == MAX_TAIL_CALL_RUNS {
                    let run = recent.pop_front().unwrap();
                    *omitted += run.count;
                }
                recent.push_back(TailCallRun {
                    label,
                    count: 1,
                });
            }
        }
    }

    fn entries(&self) -> Vec<ContextEntry> {
        match self {
            ParserContextFrame::Call { label } => {
                vec![ContextEntry::Simple(label.clone())]
            }
            ParserContextFrame::TailCalls {
                entry,
                recent,
                omitted,
            } => {
                let mut entries = Vec::new();
                let mut first_recent = 0;

                // Combine the entry with an immediately repeated call site.
                if *omitted == 0 && recent.front().map(|run| &run.label) == Some(entry) {
                    entries.push(ContextEntry::call(entry, recent[0].count + 1));
                    first_recent = 1;
                } else {
                    entries.push(ContextEntry::Simple(entry.clone()));
                }

                if *omitted > 0 {
                    entries.push(ContextEntry::Omitted { omitted: *omitted });
                }

                entries.extend(
                    recent
                        .iter()
                        .skip(first_recent)
                        .map(|run| ContextEntry::call(&run.label, run.count)),
                );
                entries
            }
        }
    }
}

/// A stack of parser context frames, tracking the call stack.
#[derive(Clone, Default)]
pub struct ParserContextStack {
    stack: Vec<ParserContextFrame>,
}

impl ParserContextStack {
    pub fn new() -> Self {
        ParserContextStack { stack: Vec::new() }
    }

    pub fn call_fun(&mut self, fun: String) {
        self.stack.push(ParserContextFrame::new(fun));
    }

    pub fn tail_call_fun(&mut self, fun: String) {
        if self.stack.is_empty() {
            self.call_fun(fun);
        } else {
            self.stack.last_mut().unwrap().tail_call(fun);
        }
    }

    pub fn pop_fun(&mut self) {
        self.stack.pop();
    }

    pub fn iter(&self) -> impl Iterator<Item = &ParserContextFrame> {
        self.stack.iter()
    }
}

// ============================================================================
// Parse Error
// ============================================================================

/// Represents a parse error with input location, message, and debug context.
#[derive(Clone)]
pub struct ParseError {
    input: ddl::Input,
    message: ddl::Array<ddl::U<8>>,
    is_system_error: bool,
    debugs: ParserContextStack,
    error_loc: String,
}

impl ParseError {
    /// Create a default (empty) parse error.
    pub fn new() -> Self {
        ParseError {
            input: ddl::new_input(ddl::new_byte_array(b""), ddl::new_byte_array(b"")),
            message: ddl::new_byte_array(b""),
            is_system_error: true,
            debugs: ParserContextStack::new(),
            error_loc: String::new(),
        }
    }

    /// Improve the current error with a new error if the new one is better.
    /// User errors take precedence over system errors.
    /// Among errors of the same type, errors at later offsets are better.
    pub fn improve(
        &mut self,
        new_is_sys: bool,
        loc: String,
        new_input: &ddl::Input,
        new_msg: ddl::ArrayB<ddl::U<8>>,
        new_debugs: &ParserContextStack,
    ) {
        // user messages take precedence over system messages
        if new_is_sys && !self.is_system_error {
            return;
        }

        // if they are the same type, then we check offsets
        // XXX: comparing offsets only really makes sense for the same input
        if new_is_sys == self.is_system_error {
            let offset = self.input.offset();
            let new_offset = new_input.offset();
            if new_offset < offset {
                return;
            }
        }

        // We found a better error.
        self.is_system_error = new_is_sys;
        self.error_loc = loc;
        self.input = new_input.clone();
        self.message = new_msg.clo();
        self.debugs = new_debugs.clone();
    }
}



// ============================================================================
// Display Implementation (Human-Readable Output)
// ============================================================================

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Convert input name bytes to string
        let name_bytes: Vec<u8> = self.input.name().iter().map(|&b| u8::from(b)).collect();
        let name = String::from_utf8_lossy(&name_bytes);

        writeln!(f, "{}:[offset {}]", name, self.input.offset())?;

        // Print message
        let msg_bytes: Vec<u8> = self.message.iter().map(|&b| u8::from(b)).collect();
        let msg = String::from_utf8_lossy(&msg_bytes);
        writeln!(f, "  • {}", msg)?;

        writeln!(f, "  • Grammar context:")?;

        for frame in self.debugs.iter() {
            write!(f, "    •")?;
            for entry in frame.entries() {
                match entry {
                    ContextEntry::Simple(name) => write!(f, " {}", name)?,
                    ContextEntry::WithCount(name, count) => {
                        write!(f, " {} ({} times)", name, count)?
                    }
                    ContextEntry::Omitted { omitted } => {
                        write!(f, " ... ({} tail calls omitted)", omitted)?
                    }
                }
            }
            writeln!(f)?;
        }

        if !self.error_loc.is_empty() {
            writeln!(f, "    • {}", self.error_loc)?;
        }

        Ok(())
    }
}

// ============================================================================
// JSON Serialization
// ============================================================================

// Helper type for serializing context entries
#[derive(Serialize)]
#[serde(untagged)]
enum ContextEntry {
    WithCount(String, usize),
    Simple(String),
    Omitted { omitted: usize },
}

impl ContextEntry {
    fn call(label: &str, count: usize) -> Self {
        if count > 1 {
            ContextEntry::WithCount(label.to_string(), count)
        } else {
            ContextEntry::Simple(label.to_string())
        }
    }
}

#[derive(Serialize)]
#[serde(untagged)]
enum ContextFrame {
    Call(String),
    TailCalls(Vec<ContextEntry>),
}

impl Serialize for ParseError {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = serializer.serialize_map(None)?;

        // Serialize error message
        let msg_bytes: Vec<u8> = self.message.iter().map(|&b| u8::from(b)).collect();
        let msg = String::from_utf8_lossy(&msg_bytes);
        map.serialize_entry("error", msg.as_ref())?;

        // Serialize offset
        map.serialize_entry("offset", &self.input.offset())?;

        // Serialize context
        let context: Vec<_> = self
            .debugs
            .iter()
            .map(|frame| match frame {
                ParserContextFrame::Call { label } => ContextFrame::Call(label.clone()),
                ParserContextFrame::TailCalls { .. } => ContextFrame::TailCalls(frame.entries()),
            })
            .collect();
        map.serialize_entry("context", &context)?;

        // Serialize location if present
        if !self.error_loc.is_empty() {
            map.serialize_entry("location", &self.error_loc)?;
        }

        map.end()
    }
}

// ============================================================================
// Parser State (for VM execution)
// ============================================================================

pub struct ParserStateWith<T> {
  #[cfg(feature = "detailed-errors")]
  context: ParserContextStack,
  pub error: ParseError,
  pub user_state: T,
}

pub type ParserState = ParserStateWith<()>;

pub fn new_parser_state_with<T>(user_state: T) -> ParserStateWith<T> {
  ParserStateWith {
    #[cfg(feature = "detailed-errors")]
    context: ParserContextStack::new(),
    error: ParseError::new(),
    user_state,
  }
}

pub fn new_parser_state() -> ParserState {
  new_parser_state_with(())
}

impl<T> ParserStateWith<T> {

  pub fn reset_parse_error(&mut self) {
    self.error = ParseError::new();

    #[cfg(feature = "detailed-errors")]
    {
      self.context = ParserContextStack::new();
    }
  }

  pub fn set_exception(&mut self, loc: &'static str, msg: &'static str) {
    #[cfg(feature = "detailed-errors")]
    let debugs = self.context.clone();

    #[cfg(not(feature = "detailed-errors"))]
    let debugs = ParserContextStack::new();

    self.error = ParseError {
      input: ddl::new_input(ddl::new_byte_array(b""), ddl::new_byte_array(b"")),
      message: ddl::new_byte_array(msg.as_bytes()),
      is_system_error: false,
      debugs,
      error_loc: loc.to_string(),
    };
  }

  pub fn say(&self, msg: &str) {
    println!("{}", msg)
  }

  #[allow(unused_variables)]
  pub fn push(&mut self, tail: bool, name: &str) {
    #[cfg(feature = "detailed-errors")]
    {
      if tail {
        self.context.tail_call_fun(name.to_string());
      } else {
        self.context.call_fun(name.to_string());
      }
    }
  }

  pub fn pop(&mut self) {
    #[cfg(feature = "detailed-errors")]
    self.context.pop_fun();
  }

  #[allow(unused_variables)]
  pub fn note_fail(&mut self, is_user: bool, loc: &str, inp: &ddl::Input, msg: ddl::ArrayB<ddl::U<8>>) {
    #[cfg(feature = "detailed-errors")]
    {
      // Improve the error if this one is better
      self.error.improve(!is_user, loc.to_string(), inp, msg, &self.context);
    }
  }
}
