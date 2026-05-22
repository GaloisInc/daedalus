use crate as ddl;
use ddl::{Type,Clo};
use std::rc::Rc;
use serde::Serialize;

/// A helper type for efficient building of arrays (owned form).
pub struct Builder<T> { node: ddl::O<Node<T>> }

/// A helper type for efficient building of arrays (borrowed form).
pub struct BuilderB<'a,T> { node: ddl::B<'a,Node<T>> }

/// Iterator over borrowed elements of a borrowed builder.
pub struct BuilderBIter<'a, T> {
    nodes: Vec<ddl::B<'a, Node<T>>>,
    node_index: usize,
    elem_index: usize,
}

impl<T> Clone for Builder<T> {
  fn clone(&self) -> Self { Builder { node: self.node.clone() } }
}

impl<'a,T> Copy for BuilderB<'a,T> {}

impl<'a,T> Clone for BuilderB<'a,T> {
  fn clone(&self) -> Self { BuilderB { node: self.node.clone() } }
}

impl <T: ddl::Type> Type for Builder<T> {
  type B<'a> = BuilderB<'a,T>;
  fn bor(&self) -> BuilderB<T> { BuilderB { node: self.node.bor() } }
}

impl <'a, T: ddl::Type> Clo for BuilderB<'a,T> {
  type O = Builder<T>;
  fn clo(self) -> Builder<T> { Builder { node: self.node.clo() } }
}

impl<'a, T: Type + Ord> Ord for BuilderB<'a, T> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.iter().cmp(other.iter())
  }
}

impl<'a, T: Type + Ord> PartialOrd for BuilderB<'a, T> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a, T: Type + Ord> PartialEq for BuilderB<'a, T> {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == std::cmp::Ordering::Equal
  }
}

impl<'a, T: Type + Ord> Eq for BuilderB<'a, T> {}

impl<T: Type + Ord> Ord for Builder<T> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.iter().cmp(other.iter())
  }
}

impl<T: Type + Ord> PartialOrd for Builder<T> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp(other))
  }
}

impl<T: Type + Ord> PartialEq for Builder<T> {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == std::cmp::Ordering::Equal
  }
}

impl<T: Type + Ord> Eq for Builder<T> {}

impl<'a, T: Type> Iterator for BuilderBIter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      if self.node_index >= self.nodes.len() {
        return None;
      }

      let node = self.nodes[self.node_index];
      let node_ref = node.as_ref();

      if self.elem_index < node_ref.data.len() {
        let item = &node_ref.data[self.elem_index];
        self.elem_index += 1;
        return Some(item);
      } else {
        // Move to the next node
        self.node_index += 1;
        self.elem_index = 0;
      }
    }
  }
}


impl<'a, T: Type> BuilderB<'a, T> {
  pub fn iter(self) -> BuilderBIter<'a, T> {
    // Collect nodes in reverse order, just like in build()
    let mut nodes = vec![];
    let mut cur = Some(self);

    while let Some(builder) = cur {
      let node = builder.node;
      nodes.push(node);
      cur = node.as_ref().more.as_ref().map(|b| b.bor());
    }

    // Reverse so we iterate oldest to newest
    nodes.reverse();

    BuilderBIter {
      nodes,
      node_index: 0,
      elem_index: 0,
    }
  }
}

struct Node<T> {
  data: Vec<T>,
  more: Option<Builder<T>>
}

pub fn new_builder<T>() -> Builder<T> {
  Builder { node: ddl::new(Node { data: vec![], more: None }) }
}


impl<T: Type> Builder<T> {
  pub fn iter(&self) -> BuilderBIter<T> {
    self.bor().iter()
  }
}

impl<T: Clone> Builder<T> {

  pub fn push(mut self, x: T) -> Builder<T> {
    match Rc::get_mut(&mut self.node.rc) {
      Some(v) => { v.data.push(x); self },
      None    => Builder { node: ddl::new (Node { data: vec![x], more: Some(self) }) }
    }
  }

  pub fn push_array(self, x: ddl::Array<T>) -> Builder<T> {
    Builder { node: ddl::new(Node { data: ddl::array_to_vec(x), more: Some(self) }) }
  }

  pub fn build(self) -> ddl::Array<T> {
    enum Chunk<T> { Owned(Vec<T>), Shared(Rc<Node<T>>) }

    let mut size = 0;
    let mut stack: Vec<Chunk<T>> = vec![];

    let mut cur = Some(self);
    while let Some(builder) = cur {
      match Rc::try_unwrap(builder.node.rc) {
        Ok(node) => {
          size += node.data.len();
          cur = node.more;
          stack.push(Chunk::Owned(node.data));
        },
        Err(rc) => {
          size += rc.data.len();
          cur = rc.more.clone();
          stack.push(Chunk::Shared(rc));
        }
      }
    }

    let mut v = Vec::with_capacity(size);
    for chunk in stack.into_iter().rev() {
      match chunk {
        Chunk::Owned(mut xs) => v.append(&mut xs),
        Chunk::Shared(rc) => v.extend_from_slice(&rc.data),
      }
    }
    ddl::new_array_vec(v)
  }
}




fn serialize_builder_iter<'a, S, T>(iter: BuilderBIter<'a, T>, serializer: S) -> Result<S::Ok, S::Error>
where
  S: serde::Serializer,
  T: Type + Serialize,
{
  use serde::ser::SerializeSeq;
  let mut seq = serializer.serialize_seq(None)?;
  for elem in iter {
    seq.serialize_element(elem)?;
  }
  seq.end()
}

impl<T: Type + Serialize> Serialize for Builder<T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serialize_builder_iter(self.iter(), serializer)
  }
}

impl<'a, T: Type + Serialize> Serialize for BuilderB<'a, T> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serialize_builder_iter(self.iter(), serializer)
  }
}

// Helper function to format a BuilderB with a custom element formatter
fn fmt_builder_b<'a, T: Type, F>(
  builder: BuilderB<'a, T>,
  f: &mut std::fmt::Formatter<'_>,
  mut fmt_elem: F,
) -> std::fmt::Result
where
  F: FnMut(&T, &mut std::fmt::Formatter<'_>) -> std::fmt::Result,
{
  write!(f, "[")?;
  let mut first = true;
  for elem in builder.iter() {
    if !first {
      write!(f, ", ")?;
    }
    first = false;
    fmt_elem(elem, f)?;
  }
  write!(f, "]")
}

impl<T: Type + std::fmt::Display> std::fmt::Display for Builder<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fmt_builder_b(self.bor(), f, |elem, f| write!(f, "{}", elem))
  }
}

impl<'a, T: Type + std::fmt::Display> std::fmt::Display for BuilderB<'a, T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fmt_builder_b(*self, f, |elem, f| write!(f, "{}", elem))
  }
}

impl<T: Type + std::fmt::Debug> std::fmt::Debug for Builder<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fmt_builder_b(self.bor(), f, |elem, f| write!(f, "{:?}", elem))
  }
}

impl<'a, T: Type + std::fmt::Debug> std::fmt::Debug for BuilderB<'a, T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    fmt_builder_b(*self, f, |elem, f| write!(f, "{:?}", elem))
  }
}