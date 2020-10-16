#include <ddl/unit.h>
#include "user_data.h"


void Node::init(int head, List tail) {
  _0 = head;
  _1 = tail;
}

int&  Node::get_head() { return _0; }
List& Node::get_tail() { _1.copy(); return _1; }

void Node::copy() {
  _1.copy();
}

void Node::free() {
  _1.free();
}

// ---------------------------------------------------------------------------

Tag::List   _List::getTag()   { return tag; }
DDL::Unit&  _List::get_Nil()  { return data._0; }
Node&       _List::get_Cons() { data._1.copy(); return data._1; }

void _List::init_Nil(DDL::Unit x) {
  tag       = Tag::Nil;
  data._0   = x;
}

void _List::init_Cons(Node x) {
  tag       = Tag::Cons;
  data._1   = x;
}

void _List::copy() {
  switch(getTag()) {
    case Tag::Nil:  return;
    case Tag::Cons: data._1.copy(); return;
  }
}

void _List::free() {
  switch(getTag()) {
    case Tag::Nil:  return;
    case Tag::Cons: data._1.free(); return;
  }
}

// ---------------------------------------------------------------------------

void List::init_Nil(DDL::Unit x) {
  allocate();
  getValue().init_Nil(x);
}

void List::init_Cons(Node x) {
  allocate();
  getValue().init_Cons(x);
}

Tag::List List::getTag()    { return getValue().getTag(); }
DDL::Unit& List::get_Nil()  { return getValue().get_Nil(); }
Node&      List::get_Cons() { return getValue().get_Cons(); }

