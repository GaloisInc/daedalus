use crate as ddl;

#[derive(Clone)]
pub struct Input {
  name:         ddl::Array<u8>,
  bytes:        ddl::Array<u8>,
  offset:       usize,
  last_offset:  usize       // Offset of end-of-input (1 past the end)
}

pub fn new_input(name: ddl::Array<u8>, bytes: ddl::Array<u8>) -> Input {
  Input {
    name: name,
    last_offset: bytes.len(),
    bytes: bytes,
    offset: 0,
  }
}

pub fn new_input_str(name: &str, bytes: &str) -> Input {
  let bs = ddl::new_array_from_slice(bytes.as_bytes());
  Input {
    name: ddl::new_array_from_slice(name.as_bytes()),
    last_offset: bs.len(),
    bytes: bs,
    offset: 0,
  }
}

impl Input {
  /// Get the name of the input
  pub fn name(&self)     -> ddl::ArrayRef<u8> { self.name.borrowed() }
  
  /// Get the current byte offset in the input.
  pub fn offset(&self)      -> usize { self.offset }

  /// Get the number of bytes in the input.
  pub fn len(&self)         -> usize { self.last_offset - self.offset }

  /// Check if the input is empty.
  pub fn is_empty(&self)    -> bool  { self.offset == self.last_offset }
  
  /// Get the first byte in input.  Assumes the input is not empty.
  pub fn head(&self)        -> u8    { self.bytes[0] }

  /// Advance the input to given number of bytes.
  pub fn advance(self, n: usize) -> Input {
    Input { offset: self.offset + std::cmp::min(n,self.len()), ..self }
  }

  /// Restrict the input to the given number of bytes.
  pub fn restrict(self, n: usize) -> Input {
    Input { last_offset: self.offset + std::cmp::min(n,self.len()), ..self }
  }

}