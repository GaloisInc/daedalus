use crate as ddl;
use ddl::{Clo,Type};

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
  pub fn name(&self)        -> ddl::ArrayB<ddl::U<8>> { self.name.bor() }
  
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