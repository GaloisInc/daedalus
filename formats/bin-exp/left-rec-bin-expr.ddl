{- an alternate encoding of an expression parser that uses the binding-power table described by @matklad in 

https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

 Our current parser does not halt on this. However, it seems like a
 reasonable grammar that would be good to support in the future.

 -}

import bin_expr_lib

-- Atoms: numeric constants and variables:
def Atom = Choose {
  const = Some $['0' .. '9'] ;
  var = Some $['a' .. 'z'] ;
}

-- Opcode: the binary opcodes
def Opcode = Choose {
  plus = ^{} ;
  minus = ^{} ;
  mult = ^{} ;
  div = ^{} ;
}

-- parsers for generating opcode values:

def Plus : Opcode = {| plus = ^{} |} 

def Minus : Opcode = {| minus = ^{} |} 

def Mult : Opcode = {| mult = ^{} |} 

def Div : Opcode = {| div = ^{} |} 

-- Op: parse an operand, return its left and right binding strengths:
def Op = 
  { $['+'] ;
    { opcode = Plus ;
      left_strength = ^ (1 : int);
      right_strength = ^ (2 : int) } } |
  { $['-'] ;
    { opcode = Minus ;
      left_strength = ^ (1 : int);
      right_strength = ^ (2 : int) } } |
  { $['*'] ;
    { opcode = Mult ;
      left_strength = ^ (3 : int);
      right_strength = ^ (4 : int) } } |
  { $['/'] ;
    { opcode = Div ;
      left_strength = ^ (3 : int);
      right_strength = ^ (4 : int) } } 

def LbExpr lb e = 
  { e is var ; ^{} } |
  { @e0 = e is bin_exp ;
    @min_strength = Min e0.bop.left_strength e0.bop.right_strength ;
    lb <= min_strength is true
  }

-- BinExp: a binary expression:
def BinExp = Choose {
  var = Token Atom ; -- expression is a variabale:
  bin_exp = {   -- expression is a binary expression:
    lhs = BinExp ; -- parse lhs of expression:
    bop = Token Op ; -- parse operation symbol:
    rhs = BinExp ; -- parse rhs of expression:

    LbExpr bop.left_strength lhs ; -- validate binding strength on lhs:
    LbExpr bop.right_strength rhs ; -- validate rhs binding strength:
  }
}

-- the entry point is BinExp
def Main = { $$ = BinExp ; END }
