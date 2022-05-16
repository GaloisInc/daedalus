{- a mostly-direct encoding of the Pratt parser in Rust described by
  @matklad in

  https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

-}

import bin_expr_lib

-- Atom: numeric constants and variables
def Atom = Choose {
  const = Some $['0' .. '9'];
  var = Some $['a' .. 'z'];
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
      left_bp = ^ (1 : int);
      right_bp = ^ (2 : int) } } |
  { $['-'] ;
    { opcode = Minus ;
      left_bp = ^ (1 : int);
      right_bp = ^ (2 : int) } } |
  { $['*'] ;
    { opcode = Mult ;
      left_bp = ^ (3 : int);
      right_bp = ^ (4 : int) } } |
  { $['/'] ;
    { opcode = Div ;
      left_bp = ^ (3 : int);
      right_bp = ^ (4 : int) } } 

-- bin_expr: a binary expression:
def BinExp min_bp = {
  -- parse the first atom:
  hd = Token Atom ;

  -- parse a sequence, where each element is
  tl = Many {
    bop = Token Op ; -- a binary operation,

    -- where each side of the operation binds tighter than the lower bound:
    min_bp <= bop.left_bp is true; 

    -- and a binary expression that binds tighter than the operation's
    -- right binding power
    nexp = BinExp bop.right_bp ;
  }
}

-- the entry point is BinExp
def Main = { $$ = BinExp 0 ; END }
