#include <cstddef>
#include <ddl/boxed.h>
#include <ddl/unit.h>

struct Node;
struct List;

namespace DDL { namespace Private {
struct List;
}}

namespace Tag {
  enum List { Nil, Cons };
}

// A boxed sum type
struct List : public DDL::Boxed<DDL::Private::List> {

  void init_Nil ();
  void init_Cons(Node n);

  Tag::List getTag();
  DDL::Unit get_Nil();      // returns owned
  Node      get_Cons();     // ditto

};


// Unboxed struct
class Node : public DDL::HasRefs {
  int  _0;
  List _1;
public:
  // "this" should be uninitialized;
  // We own the arguments.
  void init(int, List);

  int  get_head();           // returns owned
  List get_tail();           // returns owned

  // borrow this and xs
  bool operator == (Node xs); // XXX: by referene?

  void copy();
  void free();
};


// Unboxed sum
namespace DDL { namespace Private {
class List : public DDL::HasRefs {
public:

private:
  Tag::List tag;
  union Data {
    Node      _1;
    Data() {}
  } data;

public:

  // "this" should be uninitialized
  // Second argument we own.
  void init_Nil();
  void init_Cons(Node);

  Tag::List getTag();
  DDL::Unit get_Nil(); // returns owned, but maybe good to have borrow version?
  Node      get_Cons();  // ditto

  // borrow this and xs
  bool operator == (List xs);

  void copy();
  void free();
};
}}


// -----------------------------------------------------------------------------

inline
void Node::init(int head, List tail) {
  _0 = head;
  _1 = tail;
}

inline
int  Node::get_head() { return _0; }

inline
List Node::get_tail() { _1.copy(); return _1; }

inline
void Node::copy() {
  _1.copy();
}

inline
void Node::free() {
  _1.free();
}

inline
bool Node::operator == (Node n) {
  return _0 == n._0 && _1 == n._1;
}


// ---------------------------------------------------------------------------

namespace DDL {
namespace Private {

inline
Tag::List   List::getTag()   { return tag; }


inline
DDL::Unit   List::get_Nil()  { return DDL::Unit(); }


inline
Node        List::get_Cons() { data._1.copy(); return data._1; }


inline
void List::init_Nil() {
  tag       = Tag::Nil;
}


inline
void List::init_Cons(Node x) {
  tag       = Tag::Cons;
  data._1   = x;
}


inline
void List::copy() {
  switch(getTag()) {
    case Tag::Nil: break;
    case Tag::Cons: data._1.copy(); break;
  }
}


inline
void List::free() {
  switch(getTag()) {
    case Tag::Nil: break;
    case Tag::Cons: data._1.free(); break;
  }
}


inline
bool List::operator == (List xs) {
  if (tag != xs.tag) return false;
  switch (tag) {
    case Tag::Nil:  return true;
    case Tag::Cons: return data._1 == xs.data._1;
  }
  return false; // XXX: unreachable
}
}}

// ---------------------------------------------------------------------------


inline
void List::init_Nil() {
  allocate();
  getValue().init_Nil();
}


inline
void List::init_Cons(Node x) {
  allocate();
  getValue().init_Cons(x);
}


inline
Tag::List List::getTag()    { return getValue().getTag(); }


inline
DDL::Unit List::get_Nil()  { return getValue().get_Nil(); }


inline
Node      List::get_Cons() { return getValue().get_Cons(); }

