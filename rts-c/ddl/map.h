// This implementation follows closely
// "Red Black Tree in Functional Setting" by Chris Okasaki
// https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf

#ifndef DDL_MAP_H
#define DDL_MAP_H

#include <iostream>
#include <assert.h>

#include <ddl/boxed.h>
#include <ddl/maybe.h>

namespace DDL {

template <typename Key, typename Value>
class Map : HasRefs {

  // Empty reprsented as a null pointer, which is color black
  struct Node {
    using Color = bool;

    size_t ref_count;
    Color color;
    Key key;
    Value value;
    Node *left;
    Node *right;

    static const Color red = true;
    static const Color black = false;

    // own left, key, value, right
    Node(Color color, Node *left, Key key, Value value, Node *right)
      : ref_count(1), color(color)
      , key(key), value(value)
      , left(left), right(right) {
      }

    // Borrow n
    Node(Node *n)
      : ref_count(1), color(n->color)
      , key(n->key), value(n->value)
      , left(n->left), right(n->right) {
        if constexpr (hasRefs<Key>())   key.copy();
        if constexpr (hasRefs<Value>()) value.copy();
        copy(left);
        copy(right);
      }

    static void copy(Node *n) { if (n != nullptr) ++(n->ref_count); }

    static void free(Node *n) {
      if (n == nullptr) return;

      size_t r = n->ref_count;
      if (r == 1) {
        if constexpr (hasRefs<Key>())   n->key.free();
        if constexpr (hasRefs<Value>()) n->value.free();
        free(n->left);
        free(n->right);
        delete n;
      } else {
        n->ref_count = r - 1;
      }

    }

    // borrow k, borrow n
    // returns a borrowed reference (i.e., we don't change any counters)
    static Node* findNode(Key k, Node *n) {
      Node *curr = n;

      while (curr != nullptr) {
        if (k         < curr->key) curr = curr->left; else
        if (curr->key < k)         curr = curr->right; else
        return curr;
      }

      return nullptr;
    }

    // borrow n
    static bool is_black(Node *n) { return n == nullptr || n->color == black; }

    // borrow n
    static bool is_red(Node *n)   { return !is_black(n); }



    // assumes key not already in tree
    // own k, own v, own n
    static Node* insert(Key k, Value v, Node *n) {
      Node *curr  = ins(k, v, n);
      curr->color = black;
      return curr;
    }


    static void dump(int tab, Node *n) {
      if (n == nullptr) return;
      dump(tab + 2, n->left);
      for (int i = 0; i < tab; ++i) std::cout << ' ';
      std::cout << n << " ";
      std::cout << (n->color == red ? "R" : "B");
      std::cout << "(" << n->ref_count << ") ";
      std::cout << "Key:" << n->key << std::endl;
      dump(tab + 2, n->right);
    }


    // assumes the k is not in the tree
    // owns k, owns v, owns n
    // returns an owned *unique* node (i.e, node with ref_count == 1)
    static Node* ins(Key k, Value v, Node * n) {
      if (n == nullptr) return new Node(red, nullptr, k, v, nullptr);

      if (k      < n->key) return setRebalanceLeft (n, ins(k,v,n->left));
      if (n->key < k)      return setRebalanceRight(n, ins(k,v,n->right));
      else                 assert(false);
    }

    // Return a node that has the same data as `p` but is guaranteed to
    // be unique.  If `p` was already unique than we can reuse it.
    // owns p
    static Node* makeCopy(Node *p) {
      size_t ref = p->ref_count;
      if (ref == 1) return p;

      // std::cout << "copying\n";
      Node *res = new Node(p);
      free(p);
      return res;
    }

    // owns non-null n, owns unique non-null newLeft
    // returns an owned non-null unique node
    static
    Node* setRebalanceLeft(Node *n, Node *newLeft) {

      if (is_black(n) && is_red(newLeft)) {

        // left-left
        Node *sub2 = newLeft->left;
        if (is_red(sub2)) {

          Node* l = makeCopy(sub2);
          l->color = black;

          Node* r = makeCopy(n);
          r->color = black;
          r->left  = newLeft->right;

          newLeft->left = l;
          newLeft->right = r;
          return newLeft;
        }

        // left-right
        sub2 = newLeft->right;
        if (is_red(sub2)) {
          newLeft->color = black;
          newLeft->right = sub2->left;

          Node *r     = makeCopy(n);
          r->color    = black;
          r->left     = sub2->right;

          Node *res   = makeCopy(sub2);
          res->left   = newLeft;
          res->right  = r;
          return res;
        }

      }

      Node *res = makeCopy(n);
      res->left = newLeft;
      return res;
    }


    static
    Node* setRebalanceRight(Node *n, Node *newRight) {

      if (is_black(n) && is_red(newRight)) {

        // right-left
        Node *sub2 = newRight->left;
        if (is_red(sub2)) {
          Node *l  = makeCopy(n);
          l->color = black;
          l->right = sub2->left;

          newRight->color = black;
          newRight->left  = sub2->right;

          Node* res = makeCopy(sub2);
          res->left = l;
          res->right = newRight;
          return res;
        }

        // right-right
        sub2 = newRight->right;
        if (is_red(sub2)) {
          Node *l = makeCopy(n);
          l->color = black;
          l->right = newRight->left;

          Node *r = makeCopy(sub2);
          r->color = black;

          newRight->left = l;
          newRight->right = r;
          return newRight;
        }
      }

      Node *res = makeCopy(n);
      res->right = newRight;
      return res;
    }

  } *tree;

  Map(Node* p) : tree(p) {}

public:

  // Make an empty map
  Map() : tree(nullptr) {}

  // owns k, v, and this
  Map insert(Key k, Value v) { return Map(Node::insert(k,v,tree)); }

  // borrow this, borrow k
  bool contains(Key k) { return Node::findNode(k,tree) != nullptr; }

  // borrow this, borrow k, own result
  Maybe<Value> lookup(Key k) {
    Node *x = Node::findNode(k,tree);
    if (x == nullptr) return Maybe<Value>();
    if constexpr (hasRefs<Value>()) x->value.copy();
    return Maybe<Value>(x->value);
  }



  void dump() { Node::dump(0,tree); }


  void copy() { Node::copy(tree); }
  void free() { Node::free(tree); }
};





}






#endif
