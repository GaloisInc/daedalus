-- Maybe: library for maybe type
def maybeDefault default m = case m of {
  just x -> x
; nothing -> default
}
