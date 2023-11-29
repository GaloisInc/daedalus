import BSON
import Daedalus
import utf8

def Main = doc_size (Only BSON_document)

def value_size (v : BSON_value) =
  case v of
    String x      -> str_size x
    Document d    -> doc_size d
    Array d       -> doc_size d
    JavaScript x  -> str_size x
    Symbol x      -> str_size x
    _             -> 1

def doc_size (d : BSON_document) =
  for (s = 0; el in d) (s + value_size el.value)

def str_size (x : BSON_string) = length x

