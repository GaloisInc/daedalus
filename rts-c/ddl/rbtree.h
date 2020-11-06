// This implementation follows closely "Red Black Tree in Functional Setting" by Chris Okasaki
// https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf

#ifndef RBTREE_H
#define RBTREE_H

#include <iostream>
#include <optional>

template <typename Key, typename Value>
class RBT{

public:

  // Empty reprsented as a null pointer, which is color black
  class Node {
    Key key;
    Value value;
    bool color;
    Node * left;
    Node * right;

    static const bool red = true;
    static const bool black = false;

  public:
    // own k and v
    Node(Key k,Value v)
      : key(k), value(v), color(red), left(nullptr), right(nullptr) {}


    Node(Node *n)
      : key(n->key), value(n->value), color(n->color)
      , left(n->left), right(n->right) {}

    Node(bool color, Node * left, Key key, Value value, Node * right)
      : key(key), value(value), color(color), left(left), right(right) {}

    static Node *insert(Key k, Value v, Node *n) {
      Node *curr  = ins(k, v, n);
      curr->color = black;
      return curr;
    }

    static void print_keys(Node *n) {
      if (n == nullptr) return;

        print_keys(n->left);
        std::cout << "Key:" << n->key << std::endl;
        print_keys(n->right);
    }

    // borrow key, borrow node
    static bool contains(Key k, Node *n) { return findNode(k,n) != nullptr; }

    // this returns a borrowed reference to the element
    static std::optional<Value> search(Key k, Node * n){
      Node *p = findNode(k,n);
      return (p == nullptr) ? std::optional<Value>()
                            : std::optional<Value>{p->value};
    }


private:

    // borrow n
    static bool is_black(Node *n) { return n == nullptr || n->color == black; }

    // borrow n
    static bool is_red(Node *n)   { return !is_black(n); }

    static Node* findNode(Key k, Node *n) {
      Node *curr = n;

      while (curr != nullptr) {
        if (k < curr->key) curr = curr->left;  else
        if (curr->key < k) curr = curr->right; else
        return curr;
      }
      return nullptr;
    }

    static Node* ins(Key k, Value v, Node * n) {
      if (n == nullptr) return new Node(k, v);

      if (k < n->key) return balance(n, ins(k,v,n->left), n->right); else
      if (n->key < k) return balance(n, n->left,          ins(k,v,n->right));
      else return new Node(n);
    }

    static
    Node* balance(Node *me, Node *left, Node *right) {
      bool  color = me->color;
      Key   k     = me->key;
      Value v     = me->value;

      if (color == red) goto DONE;

      if (is_red(left)) {

        // left-left
        Node *sub2 = left->left;
        if (is_red(sub2)) {

          return allocate_rebalance
            ( sub2->left, sub2->key, sub2->value, sub2->right
            , left->key, left->value
            , left->right, k, v, right
            );
        }

        // left-right
        sub2 = left->right;
        if (is_red(sub2)) {
            return allocate_rebalance
              ( left->left,  left->key, left->value, sub2->left
              , sub2->key, sub2->value
              , sub2->right, k, v , right
              );
        }
      }

      if (is_red(right)) {

        // right-left
        Node *sub2 = right->left;
        if (is_red(sub2)) {
          return allocate_rebalance
            ( left, k, v , sub2->left
            , sub2->key,  sub2->value
            , sub2->right, right->key, right->value , right->right
            );
        }

        // right-right
        sub2 = right->right;
        if (is_red(sub2)) {
          return allocate_rebalance
            ( left, k, v , right->left
            , right->key, right->value
            , sub2->left,  sub2->key,  sub2->value, sub2->right
            );
        }
     }

    DONE:
      return new Node(color, left, k, v, right);
    }







    static Node* allocate_rebalance
              ( Node *a, Key x_key, Value x_value, Node *b
              , Key y_key, Value y_value
              , Node *c, Key z_key, Value z_value, Node *d
              ) {
        return new Node(red,
                  new Node(black, a, x_key, x_value, b),
                  y_key,
                  y_value,
                  new Node(black, c, z_key, z_value, d)
                  );
      }




  };

  int size;
  RBT::Node * tree;


public:
  RBT() : size(0), tree(nullptr) {}


  static RBT * insert(Key k, Value v, RBT *rbt) {
    RBT<Key, Value> * res = new RBT<Key, Value>();
    res->size = rbt->size + 1;    // XXX: only if not already present
    res->tree = RBT<Key, Value>::Node::insert(k, v, rbt->tree);
    return res;
  }

  static std::optional<Value> search(Key k, RBT *rbt) {
    return Node::search(k,rbt->tree);
  }

  void print_info() {
    std::cout << "Size: " << this->size << std::endl;
  }


};












#endif
