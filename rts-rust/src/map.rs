use std::{cmp::Ordering};
use std::rc::Rc;
use crate as ddl;
use ddl::{Type,Clo};

pub struct Map<K,V> { mp: ddl::Maybe<ddl::O<Node<K,V>>> }

#[derive(Copy)]
pub struct MapB<'a,K,V> { mp: ddl::Maybe<ddl::B<'a,Node<K,V>>> }

pub fn empty_map<K,V>() -> Map<K,V> { Map { mp: ddl::Maybe::Nothing } }

impl<K,V> Clone for Map<K,V> {
  fn clone(&self) -> Self { Map { mp: self.mp.clone() } }
}

impl<'a,K,V> Clone for MapB<'a,K,V> {
  fn clone(&self) -> Self { MapB { mp: self.mp.clone() } }
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
struct Node<K,V> {
  is_black: bool,
  key: K,
  value: V,
  left: Map<K,V>,
  right: Map<K,V>
}




impl<K: Ord + Clone, V: Clone> Map<K,V> {
  pub fn insert(self, k: K, v: V) -> Map<K,V> {
    let mut n = ins(k, v, self);
    n.to_mut().is_black = true;
    Map { mp: ddl::Maybe::Just(n.to_o()) }
  }
}

impl <'a, K, V> MapB<'a,K,V>
  where K: ddl::Type, V: ddl::Type, <K as ddl::Type>::B<'a>: Ord {

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

pub struct MapIterator<K,V> {
  stack: Vec<ddl::O<Node<K,V>>>
}

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

  pub fn ddl_done(&self) -> bool { self.stack.is_empty() }

  pub fn ddl_key(&self) -> K {
    let s = &self.stack;
    s[s.len()-1].bor().key.clo()
  }

  pub fn ddl_value(&self) -> V {
    let s = &self.stack;
    s[s.len()-1].bor().value.clo()
  }

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
