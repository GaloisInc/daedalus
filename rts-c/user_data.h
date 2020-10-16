#include <cstddef>
#include <ddl/boxed.h>

struct Node;
struct List;
struct _List;

namespace Tag {
  enum List { Nil, Cons };
};

// A boxed sum type
struct List : public DDL::Boxed<_List> {

  List() {};

  void init_Nil (DDL::Unit x);
  void init_Cons(Node n);

  Tag::List getTag();
  DDL::Unit& get_Nil(); // returns owned, but maybe good to have borrow version?
  Node& get_Cons();     // ditto
};


// Unboxed struct
class Node : public DDL::HasRefs {
  int  _0;
  List _1;
public:
  Node() {}                   // uninitialized

  // "this" should be uninitialized;
  // We own the arguments.
  void init(int head, List tail);

  int&  get_head();           // returns owned
  List& get_tail();           // returns owned

  void copy();
  void free();
};


// Unboxed sum
class _List : public DDL::HasRefs {
public:

private:
  Tag::List tag;
  union Data {
    DDL::Unit _0;
    Node      _1;
    Data() {}
  } data;

public:
  _List() {}

  // "this" should be uninitialized
  // Second argument we own.
  void init_Nil(DDL::Unit);
  void init_Cons(Node x);

  Tag::List getTag();
  DDL::Unit& get_Nil(); // returns owned, but maybe good to have borrow version?
  Node& get_Cons();  // ditto

  void copy();
  void free();
};

