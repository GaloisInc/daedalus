use std::{cmp::Ordering};
use std::fmt;
use serde::Serialize;
use crate as ddl;
use ddl::Type;
use ddl::map_iterators::{new_map_iterator, new_map_borrow_iterator};

/// An ordered map from keys of type `K` to values of type `V`.
pub struct Map<K,V> { pub(crate) mp: ddl::Maybe<ddl::O<Node<K,V>>> }

/// Borrowed reference to a map.
pub struct MapB<'a,K,V> { pub(crate) mp: ddl::Maybe<ddl::B<'a,Node<K,V>>> }

/// Creates a new empty map.
pub fn empty_map<K,V>() -> Map<K,V> { Map { mp: ddl::Maybe::Nothing } }

impl<K,V> Clone for Map<K,V> {
  fn clone(&self) -> Self { Map { mp: self.mp.clone() } }
}

impl<'a,K,V> Clone for MapB<'a,K,V> {
  fn clone(&self) -> Self { MapB { mp: self.mp.clone() } }
}

impl<'a,K,V> Copy for MapB<'a,K,V> {}

impl<'a, K: Type, V: Type> PartialEq for MapB<'a, K, V>
  where K::B<'a>: Ord, V::B<'a>: Ord {
  fn eq(&self, other: &Self) -> bool {
    self.cmp(other) == Ordering::Equal
  }
}

impl<'a, K: Type, V: Type> Eq for MapB<'a, K, V>
  where K::B<'a>: Ord, V::B<'a>: Ord {}

impl<'a, K: Type, V: Type> PartialOrd for MapB<'a, K, V>
  where K::B<'a>: Ord, V::B<'a>: Ord {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl<'a, K: Type, V: Type> Ord for MapB<'a, K, V>
  where K::B<'a>: Ord, V::B<'a>: Ord {
  fn cmp(&self, other: &Self) -> Ordering {
    let mut it1 = new_map_borrow_iterator(*self);
    let mut it2 = new_map_borrow_iterator(*other);

    loop {
      match (it1.ddl_done(), it2.ddl_done()) {
        (true, true) => return Ordering::Equal,
        (true, false) => return Ordering::Less,
        (false, true) => return Ordering::Greater,
        (false, false) => {
          // Compare keys first
          match it1.ddl_key().cmp(&it2.ddl_key()) {
            Ordering::Equal => {
              // If keys are equal, compare values
              match it1.ddl_value().cmp(&it2.ddl_value()) {
                Ordering::Equal => {
                  // Both key and value are equal, continue to next elements
                  it1 = it1.ddl_next();
                  it2 = it2.ddl_next();
                }
                other_ordering => return other_ordering
              }
            }
            other_ordering => return other_ordering
          }
        }
      }
    }
  }
}

impl<K: Type, V: Type> Type for Map<K,V> {
  type B<'a> = MapB<'a,K,V>;
  fn bor(&self) -> MapB<'_,K,V> { MapB { mp: self.mp.bor() } }
}

impl<'a, K: Type, V: Type> ddl::Clo for MapB<'a,K,V> {
  type O = Map<K,V>;
  fn clo(self) -> Map<K,V> { Map { mp: self.mp.clo() } }
}




#[derive(Clone)]
pub(crate) struct Node<K,V> {
  pub(crate) is_black: bool,
  pub(crate) key: K,
  pub(crate) value: V,
  pub(crate) left: Map<K,V>,
  pub(crate) right: Map<K,V>
}




impl<K: Ord + Clone, V: Clone> Map<K,V> {
  /// Inserts a key-value pair into the map.
  /// If the key already exists, its value is updated.
  /// Returns the updated map.
  pub fn insert(self, k: K, v: V) -> Map<K,V> {
    let mut n = ins(k, v, self);
    n.to_mut().is_black = true;
    Map { mp: ddl::Maybe::Just(n.to_o()) }
  }
}

impl <'a, K, V> MapB<'a,K,V>
  where K: ddl::Type, V: ddl::Type, <K as ddl::Type>::B<'a>: Ord {

  /// Checks whether the map contains the given key.
  pub fn contains(self, key: <K as Type>::B<'a>) -> bool {
    let mut cur = self;

    while let ddl::Maybe::Just(node) = cur.mp {
      let node_ref = node.as_ref();
      match key.cmp(&node_ref.key.bor()) {
        Ordering::Less => cur = node_ref.left.bor(),
        Ordering::Greater => cur = node_ref.right.bor(),
        Ordering::Equal => return true
      }
    }
    
    false
  }

  /// Looks up the value associated with the given key.
  /// Returns `Nothing` if the key is not in the map.
  pub fn lookup(self, key: <K as Type>::B<'a>) -> ddl::Maybe<V> {
    let mut cur = self;

    while let ddl::Maybe::Just(node) = cur.mp {
      let node_ref = node.as_ref();
      match key.cmp(&node_ref.key.bor()) {
        Ordering::Less => cur = node_ref.left.bor(),
        Ordering::Greater => cur = node_ref.right.bor(),
        Ordering::Equal => return ddl::Maybe::Just(node_ref.value.clone())
      }
    }
    
    ddl::Maybe::Nothing
  }
}

fn ins<K: Ord + Clone,V: Clone>(k: K, v: V, mp: Map<K,V>) -> ddl::Uniq<Node<K,V>> {
  match mp.mp {
    ddl::Maybe::Nothing =>
      ddl::new_uniq(Node{ is_black: false, key: k, value: v, left: empty_map(), right: empty_map() }),
    ddl::Maybe::Just(node) => {
      let mut n = node.to_uniq();
      match k.cmp(&n.key) {
        Ordering::Less => {
          let left = move_out_left(n.to_mut());
          set_rebalance_left(n, ins(k,v,left))
        }
        Ordering::Greater => {
          let right = move_out_right(n.to_mut());
          set_rebalance_right(n, ins(k,v,right))
        },
        Ordering::Equal   => {
          n.to_mut().value = v;
        n
        }
      }
    }
  }
}

fn move_out_left<K,V>(n: &mut Node<K,V>) -> Map<K,V> {
  std::mem::replace(&mut n.left, empty_map())
}

fn move_out_right<K,V>(n: &mut Node<K,V>) -> Map<K,V> {
  std::mem::replace(&mut n.right, empty_map())
}


fn set_rebalance_left<K: Clone,V: Clone>(mut n: ddl::Uniq<Node<K,V>>, mut new_left: ddl::Uniq<Node<K,V>>) -> ddl::Uniq<Node<K,V>> {

  if n.is_black && !new_left.is_black {
    let mut sub2 = move_out_left(new_left.to_mut());

    if let ddl::Maybe::Just(sub2_node) = sub2.mp {
      if !sub2_node.is_black {
        // left-left
        let mut l = sub2_node.to_uniq();
        l.to_mut().is_black = true;  
        
        let r = n.to_mut();
        r.is_black = true;
        r.left = move_out_right(new_left.to_mut());
        
        
        let p = new_left.to_mut();
        p.left  = Map { mp: ddl::Maybe::Just(l.to_o()) };
        p.right = Map { mp: ddl::Maybe::Just(n.to_o()) };
        
        return new_left
      }
    }

    sub2 = move_out_right(new_left.to_mut());
    if let ddl::Maybe::Just(sub2_node) = sub2.mp {
      if !sub2_node.is_black {
        // left-right

        let mut res = sub2_node.to_uniq();

        let nl = new_left.to_mut();
        nl.is_black = true;
        nl.right = move_out_left(res.to_mut());

        let rp = n.to_mut();
        rp.is_black = true;
        rp.left = move_out_right(res.to_mut());
        
        let resp   = res.to_mut();
        resp.left  = Map { mp: ddl::Maybe::Just(new_left.to_o()) };
        resp.right = Map { mp: ddl::Maybe::Just(n.to_o()) };
        return res
      }
    }
  }
  n.to_mut().left = Map { mp: ddl::Maybe::Just(new_left.to_o()) };
  n
}

fn set_rebalance_right<K: Clone,V: Clone>(mut n: ddl::Uniq<Node<K,V>>, mut new_right: ddl::Uniq<Node<K,V>>) -> ddl::Uniq<Node<K,V>> {
  if n.is_black && !new_right.is_black {

    let mut sub2 = move_out_left(new_right.to_mut());
    if let ddl::Maybe::Just(sub2_node) = sub2.mp {
      if !sub2_node.is_black {
        // right-left
        let mut res = sub2_node.to_uniq();

        let lp  = n.to_mut();
        lp.is_black = true;
        lp.right    = move_out_left(res.to_mut());

        let rp  = new_right.to_mut();
        rp.is_black = true;
        rp.left = move_out_right(res.to_mut());

        let p   = res.to_mut();
        p.left  = Map { mp: ddl::Maybe::Just(n.to_o()) };
        p.right = Map { mp: ddl::Maybe::Just(new_right.to_o()) };
        return res
      }
    }

    sub2 = move_out_right(new_right.to_mut());
    if let ddl::Maybe::Just(sub2_node) = sub2.mp {
      if !sub2_node.is_black {
        let lp = n.to_mut();
        lp.is_black = true;
        lp.right = move_out_left(new_right.to_mut());

        let mut rp = sub2_node.to_uniq();
        rp.to_mut().is_black = true;

        let p   = new_right.to_mut();
        p.left  = Map { mp: ddl::Maybe::Just(n.to_o()) };
        p.right = Map { mp: ddl::Maybe::Just(rp.to_o()) };
        return new_right
      }
    }
  }

  n.to_mut().right = Map { mp: ddl::Maybe::Just(new_right.to_o()) };
  n

}

impl<K: Type, V: Type> fmt::Display for Map<K,V>
  where for<'a> K::B<'a>: fmt::Display, for<'a> V::B<'a>: fmt::Display {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[| ")?;
    let mut it = new_map_borrow_iterator(self.bor());
    let mut first = true;
    while !it.ddl_done() {
      if !first {
        write!(f, ", ")?;
      }
      first = false;
      write!(f, "{} -> {}", it.ddl_key(), it.ddl_value())?;
      it = it.ddl_next();
    }
    write!(f, " |]")
  }
}

impl<'a, K: Type, V: Type> fmt::Display for MapB<'a, K, V>
  where K::B<'a>: fmt::Display, V::B<'a>: fmt::Display {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[| ")?;
    let mut it = new_map_borrow_iterator(*self);
    let mut first = true;
    while !it.ddl_done() {
      if !first {
        write!(f, ", ")?;
      }
      first = false;
      write!(f, "{} -> {}", it.ddl_key(), it.ddl_value())?;
      it = it.ddl_next();
    }
    write!(f, " |]")
  }
}

impl<K: Type + Serialize, V: Type + Serialize> Serialize for Map<K,V> {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    use serde::ser::SerializeMap;

    let mut map = serializer.serialize_map(Some(1))?;

    // Collect entries into a Vec
    let mut entries: Vec<(K, V)> = Vec::new();
    let mut it = new_map_iterator(self.clone());
    while !it.ddl_done() {
      entries.push((it.ddl_key(), it.ddl_value()));
      it = it.ddl_next();
    }

    map.serialize_entry("$$map", &entries)?;
    map.end()
  }
}

impl<'a, K: Type, V: Type> Serialize for MapB<'a, K, V>
  where K::B<'a>: Serialize, V::B<'a>: Serialize {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    use serde::ser::SerializeMap;

    let mut map = serializer.serialize_map(Some(1))?;

    // Collect entries into a Vec
    let mut entries: Vec<(K::B<'a>, V::B<'a>)> = Vec::new();
    let mut it = new_map_borrow_iterator(*self);
    while !it.ddl_done() {
      entries.push((it.ddl_key(), it.ddl_value()));
      it = it.ddl_next();
    }

    map.serialize_entry("$$map", &entries)?;
    map.end()
  }
}
