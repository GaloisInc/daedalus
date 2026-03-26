use crate as ddl;
use std::rc::Rc;

pub struct Builder<T> { node: ddl::O<Node<T>> }

#[derive(Copy)]
pub struct BuilderB<'a,T> { node: ddl::B<'a,Node<T>> }

impl<T> Clone for Builder<T> {
  fn clone(&self) -> Self { Builder { node: self.node.clone() } }
}

impl<'a,T> Clone for BuilderB<'a,T> {
  fn clone(&self) -> Self { BuilderB { node: self.node.clone() } }
}

impl <T: ddl::Type> ddl::Type for Builder<T> {
  type B<'a> = BuilderB<'a,T>;
  fn borrowed(&self) -> BuilderB<T> { BuilderB { node: self.node.borrowed() } }
}

impl <'a, T: ddl::Type> ddl::Clonable for BuilderB<'a,T> {
  type O = Builder<T>;
  fn cloned(self) -> Builder<T> { Builder { node: self.node.cloned() } }
}

struct Node<T> {
  data: Vec<T>,
  more: Option<Builder<T>>
}

pub fn new_builder<T>() -> Builder<T> {
  Builder { node: ddl::new(Node { data: vec![], more: None }) }
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
    
    let mut size = 0;
    let mut own_stack: Vec<Vec<T>> = vec![];
    let mut shared_stack: Vec<Rc<Node<T>>> = vec![];
    
    let mut cur = Some(self);
    while let Some(builder) = cur {
      match Rc::try_unwrap(builder.node.rc) {
        Ok(node) => {
          size += node.data.len();
          own_stack.push(node.data);
          cur = node.more;
        },
        Err(rc) => {
          size += rc.data.len();
          cur = rc.more.clone();
          shared_stack.push(rc);
        }
      }
    }

    let mut v = Vec::with_capacity(size);
    for mut xs in own_stack.into_iter().rev() {
      v.append(&mut xs);
    }
    for xs in shared_stack.into_iter().rev() {
      v.extend_from_slice(xs.data.as_slice())
    }
    ddl::new_array_vec(v)
  }
}