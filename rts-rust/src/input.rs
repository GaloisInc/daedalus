use crate as ddl;
use ddl::{Clo,Type};
use std::fmt;
use serde::Serialize;

#[derive(Clone)]

/// The type of inputs.  This is what we parse.
pub struct Input {
  name:         ddl::Array<ddl::U<8>>,
  bytes:        ddl::Array<ddl::U<8>>,
  offset:       usize,
  last_offset:  usize       // Offset of end-of-input (1 past the end)
}

ddl::by_ref!(Input);


pub fn new_input(name: ddl::Array<ddl::U<8>>, bytes: ddl::Array<ddl::U<8>>) -> Input {
  Input {
    name: name,
    last_offset: bytes.len(),
    bytes: bytes,
    offset: 0,
  }
}

pub fn new_input_str(name: &str, bytes: &str) -> Input {
  let bs = ddl::new_byte_array(bytes.as_bytes());
  Input {
    name: ddl::new_byte_array(name.as_bytes()),
    last_offset: bs.len(),
    bytes: bs,
    offset: 0,
  }
}

impl Input {
  /// Get the name of the input
  pub fn name(&self)        -> ddl::ArrayB<'_, ddl::U<8>> { self.name.bor() }
  
  /// Get the current byte offset in the input.
  pub fn offset(&self)      -> usize { self.offset }

  /// Get the bytes of the input.
  pub fn bytes(&self) -> ddl::Array<ddl::U<8>> {
    if self.len() == self.bytes.len() {
      self.bytes.clo()
    } else {
      let x = &self.bytes;
      ddl::new_array_slice(&x[self.offset .. self.last_offset])
    }
  }

  /// Get the number of bytes in the input.
  pub fn len(&self)         -> usize { self.last_offset - self.offset }

  /// Check if the input is empty.
  pub fn is_empty(&self)    -> bool  { self.offset == self.last_offset }
  
  /// Get the first byte in input.  Assumes the input is not empty.
  pub fn head(&self)        -> ddl::U<8>  { self.bytes[self.offset] }

  /// Advance the input to given number of bytes.
  /// If there are not enough bytes go to the end of the input.
  pub fn advance(self, n: usize) -> Input {
    Input { offset: self.offset + std::cmp::min(n,self.len()), ..self }
  }

  /// Advance the input by this much, if there's enough space.
  pub fn advance_maybe(self, n: usize) -> ddl::Maybe<Input> {
    if self.len() < n { return ddl::Maybe::Nothing }
    ddl::Maybe::Just(Input { offset: self.offset + n, ..self })
  }

  /// Restrict the input to the given number of bytes.
  pub fn restrict(self, n: usize) -> Input {
    Input { last_offset: self.offset + std::cmp::min(n,self.len()), ..self }
  }

  pub fn is_prefix(&self, xs: ddl::ArrayB<ddl::U<8>>) -> bool {
    let bs: &[ddl::U<8>] = &xs;
    let str_len = self.len();
    let bs_len  = bs.len();
    if str_len < bs_len { return false }
    let str: &[ddl::U<8>] = &self.bytes[self.offset .. self.offset + bs_len];
    return str == bs;
  }
}

// Helper to convert name bytes to a string for display
fn name_to_string(name: &[ddl::U<8>]) -> String {
  String::from_utf8_lossy(&name.iter().map(|&b| u8::from(b)).collect::<Vec<_>>()).to_string()
}

impl fmt::Display for Input {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let name_str = name_to_string(&self.name);
    write!(f, "Input(\"{}:0x{:x}--0x{:x}\")", name_str, self.offset, self.last_offset)
  }
}

impl fmt::Debug for Input {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let name_str = name_to_string(&self.name);
    write!(f, "Input(\"{}:0x{:x}--0x{:x}\")", name_str, self.offset, self.last_offset)
  }
}

impl Serialize for Input {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    use serde::ser::SerializeMap;

    let name_str = name_to_string(&self.name);
    let value = format!("{}:0x{:x}--0x{:x}", name_str, self.offset, self.last_offset);

    let mut map = serializer.serialize_map(Some(1))?;
    map.serialize_entry("$$input", &value)?;
    map.end()
  }
}