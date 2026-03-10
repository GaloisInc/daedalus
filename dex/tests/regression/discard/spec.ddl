-- Test: boxed (recursive) union case dispatch.
import Daedalus

def IntList = First
  nil  = { @b = UInt8; b == 0 is true }
  cons = { val = UInt8; rest = IntList }

def Main = IntList
