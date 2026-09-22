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
  pub fn ddl_val(&self) -> V {
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
pub struct MapIteratorB<'a, K, V> {
  stack: Vec<ddl::B<'a, Node<K,V>>>
}

/// Creates a new iterator for traversing a borrowed map in ascending key order.
pub fn new_map_borrow_iterator<'a, K: Type, V: Type>(mp: MapB<'a, K, V>) -> MapIteratorB<'a, K, V> {
  let empty = MapIteratorB { stack: Vec::with_capacity(4) };
  empty.push(mp)
}

impl<'a, K, V> Clone for MapIteratorB<'a, K, V> {
  fn clone(&self) -> Self { MapIteratorB { stack: self.stack.clone() } }
}

impl<K: Type, V: Type> Type for MapIterator<K,V> {
  type B<'a> = MapIteratorB<'a,K,V>;

  fn bor(&self) -> MapIteratorB<'_,K,V> {
    MapIteratorB {
      stack: self.stack.iter().map(|node| node.bor()).collect()
    }
  }
}

impl<'a, K: Type, V: Type> Clo for MapIteratorB<'a, K, V> {
  type O = MapIterator<K,V>;

  fn clo(self) -> MapIterator<K,V> {
    MapIterator {
      stack: self.stack.into_iter().map(|node| node.clo()).collect()
    }
  }
}

impl<'a, K: Type, V: Type> MapIteratorB<'a, K, V> {
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
  pub fn ddl_val(&self) -> V::B<'a> {
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
