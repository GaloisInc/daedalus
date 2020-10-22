// This implementation follows closely "Red Black Tree in Functional Setting" by Chris Okasaki
// https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf

#ifndef RBTREE_H
#define RBTREE_H

#include <string>
#include <optional>

using std::string;
using std::optional;


template <typename Key, typename Value>
class RBT{

public:
  class Node {
    Key key;
    Value value;
    bool color;
    Node * left;
    Node * right;

  public:
    Node(Key,Value);
    Node(Node *);
    Node(bool color, Node * left, Key key, Value value, Node * right);

    static bool is_red(Node *);
    static bool is_black(Node *);
    static Node * insert(Key k, Value v, Node *);

    static std::optional<Value> search(Key k, Node *);
    static void print_keys(Node *);
    static const bool red = true;
    static const bool black = false;

  public:
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
  RBT();

  static RBT * insert(Key k, Value v, RBT *);
  static std::optional<Value> search(Key k, RBT *);
  void print_info();
};



#endif
