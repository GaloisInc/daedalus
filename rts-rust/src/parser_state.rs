use crate as ddl;
use ddl::Clo;
use std::collections::HashMap;
use std::fmt;
use serde::Serialize;

// ============================================================================
// Parser Context Tracking
// ============================================================================

/// A frame in the parser context stack, tracking the current function
/// and history of tail calls within this frame.
#[derive(Clone)]
pub struct ParserContextFrame {
    cur: String,
    history: HashMap<String, usize>,
}

impl ParserContextFrame {
    pub fn new(fun: String) -> Self {
        ParserContextFrame {
            cur: fun,
            history: HashMap::new(),
        }
    }

    pub fn tail_call(&mut self, fun: String) {
        let count = self.history.entry(self.cur.clone()).or_insert(0);
        *count += 1;
        self.cur = fun;
    }

    pub fn get_cur(&self) -> &str {
        &self.cur
    }

    pub fn get_history(&self) -> &HashMap<String, usize> {
        &self.history
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
            write!(f, "    • {}", frame.get_cur())?;

            let history = frame.get_history();
            if let Some(&count) = history.get(frame.get_cur()) {
                if count > 0 {
                    write!(f, " ({} times)", count + 1)?;
                }
            }

            for (name, &count) in history.iter() {
                if name == frame.get_cur() {
                    continue;
                }
                write!(f, " {}", name)?;
                if count > 1 {
                    write!(f, " ({} times)", count)?;
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
        let mut context: Vec<Vec<ContextEntry>> = Vec::new();
        for frame in self.debugs.iter() {
            let mut frame_entries = Vec::new();

            let cur = frame.get_cur();
            let history = frame.get_history();

            // Current function (first in frame)
            let n = if let Some(&count) = history.get(cur) {
                count + 1
            } else {
                1
            };

            if n > 1 {
                frame_entries.push(ContextEntry::WithCount(cur.to_string(), n));
            } else {
                frame_entries.push(ContextEntry::Simple(cur.to_string()));
            }

            // History entries (excluding current)
            for (name, &count) in history.iter() {
                if name == cur {
                    continue;
                }
                if count > 1 {
                    frame_entries.push(ContextEntry::WithCount(name.clone(), count));
                } else {
                    frame_entries.push(ContextEntry::Simple(name.clone()));
                }
            }

            context.push(frame_entries);
        }
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

pub struct ParserState {
  #[cfg(feature = "detailed-errors")]
  context: ParserContextStack,
  pub error: ParseError,
}

pub fn new_parser_state() -> ParserState {
  ParserState {
    #[cfg(feature = "detailed-errors")]
    context: ParserContextStack::new(),
    error: ParseError::new(),
  }
}

impl ParserState {

  pub fn set_exception(&mut self, loc: &'static str, msg: &'static str) {
    self.error = ParseError {
      input: ddl::new_input(ddl::new_byte_array(b""), ddl::new_byte_array(b"")),
      message: ddl::new_byte_array(msg.as_bytes()),
      is_system_error: false,
      debugs: ParserContextStack::new(),
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