// This implementation follows closely "Red Black Tree in Functional Setting" by Chris Okasaki
// https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf

#include <iostream>
#include <string>
#include <optional>
#include <cassert>

#include "rbtree.hpp"

using namespace std;
using std::optional;




template<typename Key, typename Value>
typename RBT<Key,Value>::Node *
RBT<Key,Value>::Node::allocate_rebalance(Node * a,
                                       Key   x_key,
                                       Value x_value,
                                       Node * b,
                                       Key   y_key,
                                       Value y_value,
                                       Node * c,
                                       Key   z_key,
                                       Value z_value,
                                       Node * d) {
  return new Node(red,
                  new Node(black, a, x_key, x_value, b),
                  y_key,
                  y_value,
                  new Node(black, c, z_key, z_value, d)
                  );
}

template<typename Key, typename Value>
typename RBT<Key,Value>::Node *
RBT<Key,Value>::Node::balance(bool color, Node * left, Key k, Value v, Node * right){
  Node * a;
  Node * b;
  Node * c;
  Node * d;
  Key   z_key;
  Value z_value;
  Key   x_key;
  Value x_value;
  Key   y_key;
  Value y_value;

  Node * sub1;
  Node * sub2;

  // cout << "Balance In" << endl;
  // balance B (T R (T R a x b) y c) z d =
  if (color == black) {
    sub1 = left;
    if (sub1 != nullptr) {
      sub2 = sub1->left;
      if (sub2 != nullptr) {
        if (is_red(sub1) && is_red(sub2)) {
          a       = sub2->left;
          x_key   = sub2->key;
          x_value = sub2->value;
          b       = sub2->right;
          y_key   = sub1->key;
          y_value = sub1->value;
          c       = sub1->right;
          z_key   = k;
          z_value = v;
          d       = right;

          return allocate_rebalance(a, x_key, x_value, b, y_key, y_value, c, z_key, z_value, d);
        }
      }
    }
  }

  if (color == black) {
    sub1 = left;
    if (left != nullptr) {
      sub2 = sub1->right;
      if (sub1->right != nullptr) {
        if (is_red(sub1) && is_red(sub2)) {
          a       = sub1->left;
          x_key   = sub1->key;
          x_value = sub1->value;
          b       = sub2->left;
          y_key   = sub2->key;
          y_value = sub2->value;
          c       = sub2->right;
          z_key   = k;
          z_value = v;
          d       = right;
          return allocate_rebalance(a, x_key, x_value, b, y_key, y_value, c, z_key, z_value, d);
        }
      }
    }
  }

  if (color == black) {
    sub1 = right;
    if (right != nullptr) {
      sub2 = sub1->left;
      if (sub2 != nullptr) {
        if (is_red(sub1) && is_red(sub2)) {
          a       = left;
          x_key   = k;
          x_value = v;
          b       = sub2->left;
          y_key   = sub2->key;
          y_value = sub2->value;
          c       = sub2->right;
          z_key   = sub1->key;
          z_value = sub1->value;
          d       = sub1->right;
          return allocate_rebalance(a, x_key, x_value, b, y_key, y_value, c, z_key, z_value, d);
        }
      }
    }
  }

  if (color == black) {
    sub1 = right;
    if (right != nullptr) {
      sub2 = sub1->right;
      if (sub2 != nullptr) {
        if (is_red(sub1) && is_red(sub2)) {
          a       = left;
          x_key   = k;
          x_value = v;
          b       = sub1->left;
          y_key   = sub1->key;
          y_value = sub1->value;
          c       = sub2->left;
          z_key   = sub2->key;
          z_value = sub2->value;
          d       = sub2->right;
          return allocate_rebalance(a, x_key, x_value, b, y_key, y_value, c, z_key, z_value, d);
        }
      }
    }
  }

  return new Node(color, left, k, v, right);
}

template<typename Key, typename Value>
typename RBT<Key,Value>::Node * RBT<Key,Value>::Node::ins(Key k, Value v,  RBT<Key,Value>::Node * n){

  Node * sub;

  if (n == nullptr) {
    return new Node(k, v);
  } else {
    if (k < n->key) {
      sub = ins(k, v, n->left);
      return balance(n->color, sub, n->key, n->value, n->right);
    } else if (k > n->key) {
      sub = ins(k, v, n->right);
      return balance(n->color, n->left, n->key, n->value, sub);
    } else { // it is equal
      sub = new Node(n);
      assert (Node::is_red(sub));
      return sub;
    }
  }
}

template<typename Key, typename Value>
typename RBT<Key,Value>::Node * RBT<Key,Value>::Node::insert(Key k, Value v,  RBT<Key,Value>::Node * n){
  Node * curr;
  curr = ins(k, v, n);
  curr->color = black;
  return curr;
}

template<typename Key, typename Value>
void RBT<Key, Value>::Node::print_keys(Node * n){
  if (n == nullptr)
    return;

  print_keys(n->left);
  cout << "Key:" << n->key << endl;
  print_keys(n->right);
}

template<typename Key, typename Value>
std::optional<Value> RBT<Key,Value>::Node::search(Key k, Node * n){
  Node * curr = n;

  while (curr != nullptr) {
    if (k < curr->key) {
      curr = curr->left;
    } else if (k > curr->key){
      curr = curr->right;
    } else { // key == k
      return std::optional<Value>{curr->value};
    }
  }
  return {};
}



int main(){
  RBT<int,int> * t = new RBT<int,int>();
  t->print_info();

  t = RBT<int,int>::insert(3, 17, t);
  t->print_info();
  RBT<int,int>::Node::print_keys(t->tree);

  t = RBT<int,int>::insert(2, 18, t);
  t->print_info();;
  RBT<int,int>::Node::print_keys(t->tree);

  t = RBT<int,int>::insert(1, 19, t);
  t->print_info();;
  RBT<int,int>::Node::print_keys(t->tree);

  RBT<int,string> * u = new RBT<int,string>();

  u = RBT<int,string>::insert(10, "Ten", u);
  u = RBT<int,string>::insert(5, "Five", u);
  u = RBT<int,string>::insert(12, "Twelve", u);
  u = RBT<int,string>::insert(7, "Seven", u);
  u = RBT<int,string>::insert(4, "Four", u);
  u = RBT<int,string>::insert(1, "One", u);
  u = RBT<int,string>::insert(8, "Eight", u);
  u = RBT<int,string>::insert(3, "Three", u);
  u = RBT<int,string>::insert(2, "Two", u);
  u = RBT<int,string>::insert(9, "Nine", u);
  u = RBT<int,string>::insert(11, "Eleven", u);
  u = RBT<int,string>::insert(27, "Twen7", u);
  u = RBT<int,string>::insert(28, "Twen8", u);
  u = RBT<int,string>::insert(22, "Twen2", u);
  u = RBT<int,string>::insert(18, "Eighteen", u);
  u = RBT<int,string>::insert(25, "Twen5", u);
  u = RBT<int,string>::insert(30, "Thirty", u);


  RBT<int,string>::Node::print_keys(u->tree);

  cout << RBT<int,string>::search(5, u).value_or("None") << endl;
  cout << RBT<int,string>::search(10, u).value_or("None") << endl;
  cout << RBT<int,string>::search(6, u).value_or("6 not Found!!!") << endl;
  cout << RBT<int,string>::search(7, u).value_or("None") << endl;
  cout << RBT<int,string>::search(11, u).value_or("None") << endl;

  return 0;
}
