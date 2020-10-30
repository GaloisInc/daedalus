// This implementation follows closely "Red Black Tree in Functional Setting" by Chris Okasaki
// https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf

#ifndef RBTREE_H
#define RBTREE_H

#include <optional>

using std::optional;


template <typename Key, typename Value>
class RBT{

public:

  // Empty reprsented as a null pointer
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

    static Node * insert(Key k, Value v, Node *);
    static std::optional<Value> search(Key k, Node *);
    static void print_keys(Node *);

private:

    // borrow n
    static bool is_black(Node *n) { return n == nullptr || n->color == black; }

    // borrow n
    static bool is_red(Node *n)   { return !is_black(n); }



    static Node * allocate_rebalance(Node * a,
                                   Key   x_key,
                                   Value x_value,
                                   Node * b,
                                   Key   y_key,
                                   Value y_value,
                                   Node * c,
                                   Key   z_key,
                                   Value z_value,
                                   Node * d);
    static Node * ins(Key k, Value v, Node * n);
    static Node * balance(bool, Node *, Key, Value, Node *);
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
