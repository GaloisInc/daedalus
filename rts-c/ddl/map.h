// This implementation follows closely
// "Red Black Tree in Functional Setting" by Chris Okasaki
// https://www.cs.tufts.edu/~nr/cs257/archive/chris-okasaki/redblack99.pdf

#ifndef DDL_MAP_H
#define DDL_MAP_H

#include <assert.h>
#include <ddl/debug.h>
#include <ddl/boxed.h>
#include <ddl/maybe.h>

namespace DDL {

template <typename Key, typename Value>
class Map : HasRefs {

  // Empty reprsented as a null pointer, which is color black
  struct Node {
    using Color = bool;

    RefCount ref_count;
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

      RefCount r = n->ref_count;
      if (r == 1) {
        debugLine("freeing last ref");
        if constexpr (hasRefs<Key>())   n->key.free();
        if constexpr (hasRefs<Value>()) n->value.free();
        free(n->left);
        free(n->right);
        delete n;
      } else {
        debugLine("freeing decrement");
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



    // own k, own v, own n
    static Node* insert(Key k, Value v, Node *n) {
      Node *curr  = ins(k, v, n);
      curr->color = black;
      return curr;
    }


    static void dump(int tab, Node *n) {
      if (n == nullptr) return;
      dump(tab + 2, n->left);
      for (int i = 0; i < tab; ++i) debug(" ");
      debugVal((void*)n); debug(" ");
      debug(n->color == red ? "R" : "B");
      debug("("); debugVal(n->ref_count); debug(") ");
      debug("Key:"); debugNL(n->key);
      dump(tab + 2, n->right);
    }

    // Return a node that has the same data as `p` but is guaranteed to
    // be unique.  If `p` was already unique than we can reuse it.
    // owns p
    static Node* makeCopy(Node *p) {
      RefCount ref = p->ref_count;
      if (ref == 1) return p;

      Node *res = new Node(p);
      free(p);
      return res;
    }




    // assumes the k is not in the tree
    // owns k, owns v, owns n
    // returns an owned *unique* node (i.e, node with ref_count == 1)
    static Node* ins(Key k, Value v, Node * n) {
      if (n == nullptr) return new Node(red, nullptr, k, v, nullptr);

      if (k      < n->key) return setRebalanceLeft (n, ins(k,v,n->left));
      if (n->key < k)      return setRebalanceRight(n, ins(k,v,n->right));
      else {
        Node *res = makeCopy(n);
        if constexpr (hasRefs<Value>()) n->value.free();
        n->value = v;
        return res;
      }
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


    friend
    std::ostream& operator << (std::ostream &os, Node *n) {
      if (n == nullptr) return os;
      os << n->left;
      if (n->left != nullptr) os << ", ";
      os << n->key << " -> " << n->value;
      if (n->right != nullptr) os << ", ";
      os << n->right;
      return os;
    }



  } *tree;

  Map(Node* p) : tree(p) {}


public:

  class Iterator : HasRefs {
    Boxed<Iterator> above;
    Node           *cur;


    // Owns up, p
    // Assumes `p` is not nullptr
    // Build an iterator chain going all the way left.
    static
    Iterator goLeft(Boxed<Iterator> next, Node *p) {
      Iterator here(next, p);

      Node *l = p->left;
      while (l) {
        Boxed<Iterator> up(here);
        Node::copy(l);
        here.above = up;
        here.cur = l;
        l = l->left;
      }
      return here;
    }

    // Owns above and me
    Iterator (Node *me) : above(Boxed<Iterator>()), cur(me) {}
    Iterator (Boxed<Iterator> up, Node *me) : above(up), cur(me) {}

  public:
    Iterator() : above(), cur(nullptr) {}

    // Owns argument
    Iterator (Map m) {
      if (m.tree != nullptr) *this = goLeft(Boxed<Iterator>(), m.tree);
      else cur = nullptr;
    }

    // borrow this
    bool  done()        { return cur == nullptr; }

    // borrow this
    Key   borrowKey()   { return cur->key; }

    // borrow this
    Value borrowValue() { return cur->value; }

    // owns this
    Iterator next() {
      if (cur->right != nullptr) {
        Node::copy(cur->right);
        Iterator r = goLeft(above,cur->right);
        Node::free(cur);
        return r;
      }
      Node::free(cur);
      if (above.isNull()) return Iterator();
      Iterator r = above.getValue();
      if (above.refCount() != 1) r.copy();
      above.shallowFree();
      return r;
    }

    void copy() {
      if (!above.isNull()) above.copy();
      if (cur != nullptr) Node::copy(cur);
    }

    void free() {
      if (!above.isNull()) above.free();
      if (cur != nullptr) Node::free(cur);
    }

    void dump() {
      debug("IT:"); debugVal((void*) cur); debug("|");
      above.dump();
      if (!above.isNull()) above.getValue().dump();
      debugLine("---");
    }

  };


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

  // reference counting
  void copy() { Node::copy(tree); }
  void free() { Node::free(tree); }

  // for debugging
  void dump() { Node::dump(0,tree); debugNL(); }

  friend
  // borrow m
  std::ostream& operator << (std::ostream &os, Map m) {
    os << "[| " << m.tree << " |]";
    return os;
  }

};



template <typename Key, typename Value>
static inline
int compare(Map<Key,Value> m1, Map<Key,Value> m2) {
  m1.copy();
  m2.copy();
  typename Map<Key,Value>::Iterator it1(m1);
  typename Map<Key,Value>::Iterator it2{m2};
  int result;
  while (! (it1.done() || it2.done())) {
    Key k1 = it1.borrowKey();
    Key k2 = it2.borrowKey();
    result = compare(k1,k2);
    if (result != 0) goto end;

    Value v1 = it1.borrowValue();
    Value v2 = it2.borrowValue();
    result = compare(v1,v2);
    if (result != 0) goto end;

    it1 = it1.next();
    it2 = it2.next();
  }
  result = it1.done() ? !it2.done() : -1;

end:
  it1.free();
  it2.free();
  return result;
}


// Borrow arguments
template <typename Key, typename Value> static inline
bool operator == (Map<Key,Value> xs, Map<Key,Value> ys) {
  return compare(xs,ys) == 0;
}

// Borrow arguments
template <typename Key, typename Value> static inline
bool operator < (Map<Key,Value> xs, Map<Key,Value> ys) {
  return compare(xs,ys) < 0;
}

// Borrow arguments
template <typename Key, typename Value> static inline
bool operator > (Map<Key,Value> xs, Map<Key,Value> ys) {
  return compare(xs,ys) > 0;
}

// Borrow arguments
template <typename Key, typename Value> static inline
bool operator != (Map<Key,Value> xs, Map<Key,Value> ys) { return !(xs == ys); }

// Borrow arguments
template <typename Key, typename Value> static inline
bool operator <= (Map<Key,Value> xs, Map<Key,Value> ys) { return !(xs > ys); }

// Borrow arguments
template <typename Key, typename Value> static inline
bool operator >= (Map<Key,Value> xs, Map<Key,Value> ys) { return !(xs < ys); }


// borrow
template <typename Key, typename Value>
inline
std::ostream& toJS(std::ostream& os, Map<Key,Value> x) {
  os << "{ \"$$map\":";
  char sep = '[';
  x.copy();
  typename Map<Key,Value>::Iterator it(x);
  if (it.done()) { os << "["; goto end; }
  do {
    os << sep << "[";
    toJS(os,it.borrowKey());
    os << ",";
    toJS(os,it.borrowValue());
    os << "]";
    sep = ',';
    it = it.next();
  } while(!it.done());

end:
  it.free();
  os << "]}";
  return os;
}


}



#endif
