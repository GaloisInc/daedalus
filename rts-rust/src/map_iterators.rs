use std::rc::Rc;
use crate as ddl;
use ddl::{Type, Clo};
use ddl::map::{Map, MapB, Node, empty_map};

/// An iterator that traverses a map in ascending key order.
/// Consumes the map during iteration.
pub struct MapIterator<K,V> {
  stack: Vec<ddl::O<Node<K,V>>>
}

/// Creates a new iterator for traversing the map in ascending key order.
pub fn new_map_iterator<K: Type, V: Type>(mp: Map<K,V>) -> MapIterator<K, V> {
  let empty = MapIterator { stack: Vec::with_capacity(4) };
  empty.push(mp)
}

impl<K,V> Clone for MapIterator<K,V> {
  fn clone(&self) -> Self { MapIterator { stack: self.stack.clone() } }
}

ddl::by_ref!(MapIterator<K,V>);


impl<K: Type, V: Type> MapIterator<K,V> {
  fn push(self, mp: Map<K,V>) -> Self {
    let mut res = self;
    let mut cur = mp;
    while let ddl::Maybe::Just(mut nd) = cur.mp {
      let l =
        match Rc::get_mut(&mut nd.rc) {
          None      => nd.left.clone(),
          Some(r)   => std::mem::replace(&mut r.left, empty_map())
        };
      res.stack.push(nd);
      cur = l;
    }
    res
  }

  /// Returns `true` if there are no more elements to iterate over.
  pub fn ddl_done(&self) -> bool { self.stack.is_empty() }

  /// Returns the key at the current iterator position.
  /// Should only be called when `!self.ddl_done()`.
  pub fn ddl_key(&self) -> K {
    let s = &self.stack;
    s[s.len()-1].bor().key.clo()
  }

  /// Returns the value at the current iterator position.
  /// Should only be called when `!self.ddl_done()`.
  pub fn ddl_value(&self) -> V {
    let s = &self.stack;
    s[s.len()-1].bor().value.clo()
  }

  /// Advances the iterator to the next element in ascending key order.
  /// Should only be called when `!self.ddl_done()`.
  pub fn ddl_next(mut self) -> Self {
    let mut nd = self.stack.pop().unwrap();
    self.push(
      match Rc::get_mut(&mut nd.rc) {
        None    => nd.right.clone(),
        Some(r) => std::mem::replace(&mut r.right, empty_map())
      }
    )
  }
}

/// An iterator that traverses a borrowed map in ascending key order.
/// Borrows the map during iteration without consuming it.
pub struct MapBorrowIterator<'a, K, V> {
  stack: Vec<ddl::B<'a, Node<K,V>>>
}

/// Creates a new iterator for traversing a borrowed map in ascending key order.
pub fn new_map_borrow_iterator<'a, K: Type, V: Type>(mp: MapB<'a, K, V>) -> MapBorrowIterator<'a, K, V> {
  let empty = MapBorrowIterator { stack: Vec::with_capacity(4) };
  empty.push(mp)
}

impl<'a, K, V> Clone for MapBorrowIterator<'a, K, V> {
  fn clone(&self) -> Self { MapBorrowIterator { stack: self.stack.clone() } }
}

impl<'a, K: Type, V: Type> MapBorrowIterator<'a, K, V> {
  fn push(self, mp: MapB<'a, K, V>) -> Self {
    let mut res = self;
    let mut cur = mp;
    while let ddl::Maybe::Just(nd) = cur.mp {
      let l = nd.as_ref().left.bor();
      res.stack.push(nd);
      cur = l;
    }
    res
  }

  /// Returns `true` if there are no more elements to iterate over.
  pub fn ddl_done(&self) -> bool { self.stack.is_empty() }

  /// Returns a borrowed reference to the key at the current iterator position.
  /// Should only be called when `!self.ddl_done()`.
  pub fn ddl_key(&self) -> K::B<'a> {
    let s = &self.stack;
    s[s.len()-1].as_ref().key.bor()
  }

  /// Returns a borrowed reference to the value at the current iterator position.
  /// Should only be called when `!self.ddl_done()`.
  pub fn ddl_value(&self) -> V::B<'a> {
    let s = &self.stack;
    s[s.len()-1].as_ref().value.bor()
  }

  /// Advances the iterator to the next element in ascending key order.
  /// Should only be called when `!self.ddl_done()`.
  pub fn ddl_next(mut self) -> Self {
    let nd = self.stack.pop().unwrap();
    self.push(nd.as_ref().right.bor())
  }
}
