def Main =
  block
    emptyMap = empty : [int -> int]
    nonEmptyMap = insert 1 2 emptyMap
    emptyMapIsEmpty = isMapEmpty emptyMap
    nonEmptyMapIsEmpty = isMapEmpty nonEmptyMap
