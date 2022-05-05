{-# Language OverloadedStrings, QuasiQuotes #-}
module Daedalus.Compile.ConfigParser (pConfig) where

import Daedalus.Quote

[daedalus| Config

def Config =
  block
    Many Space
    header  = Header
    modules = Many Module
    END

def Module =
  block
    KW "module"
    name = Name
    KW "where"
    header  = Header
    prims   = Many PrimDecl

def Header =
  block
    imports = Many Import
    monad   = Optional MonadDecl

def Import =
  block
    KW "import"
    module = Name
    qualifier =
      Optional
        block
          KW "as"
          Name

def MonadDecl =
  block
    KW "monad"
    Term

def PrimDecl =
  block
    KW "def"
    name       = Name
    args       = Many Name
    KW "="
    definition = Term

def Term =
  block
    fun  = Name
    args = Many Arg

def Arg =
  First
    TArg = block
             $['@']
             String
    Arg  = ATerm

def ATerm : Term =
  First

    block
      fun  = Name
      args = []

    block
      KW "("
      $$ = Term
      KW ")"



--------------------------------------------------------------------------------
-- Lexical Structure

def $alpha        = 'A' .. 'Z' | 'a' .. 'z'
def $name_char    = $alpha | '0' .. '9' | "_."
def $space        = " \n\r\t"
def $string_char  = '"'

def Comment =
  block
    Match "--"
    @Many $[!'\n']

def Space = Comment <| @$space

def Token P =
  block
    $$ = P
    Many Space

def KW x = @Token (Match x)

def Name =
  block
    $$ = Token (build (many (b = emit builder $alpha) (emit b $name_char)))
    case $$ of
      "def", "module", "where", "monad" -> Fail "keyword"
      _                                 -> Accept

def String =
  Token
    block
      $string_char
      $$ = build (many (b = builder) (StringChar b))
      $string_char

def StringChar b =
  First
    block
      $['\\']
      case UInt8 of
        '"'  -> emit b '"'
        '\\' -> emit b '\\'
        'n'  -> emit b '\n'
        'r'  -> emit b '\r'
        't'  -> emit b '\t'
        '\n' -> b

    emit b $[!'"']



|]



