-- Maybe: library for maybe type
def maybeDefault dfault m = case m of {
  just x -> x
; nothing -> dfault
}
