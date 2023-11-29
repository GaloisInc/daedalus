meta:
  id: vlq_base128_le
  title: Variable length quantity
  license: CC0-1.0
  ks-version: 0.7
seq:
  - id: groups
    type: group
    repeat: until
    repeat-until: not _.has_next
types:
  group:
    seq:
      - id: b
        type: u1
    instances:
      has_next:
        value: (b & 0b1000_0000) != 0
      value:
        value: b & 0b0111_1111
instances:
  len:
    value: groups.size
  value:
    value: >-
      (groups[0].value
      + (len >= 2 ? (groups[1].value.as<u8> << 7) : 0)
      + (len >= 3 ? (groups[2].value.as<u8> << 14) : 0)
      + (len >= 4 ? (groups[3].value.as<u8> << 21) : 0)
      + (len >= 5 ? (groups[4].value.as<u8> << 28) : 0)
      + (len >= 6 ? (groups[5].value.as<u8> << 35) : 0)
      + (len >= 7 ? (groups[6].value.as<u8> << 42) : 0)
      + (len >= 8 ? (groups[7].value.as<u8> << 49) : 0)).as<u8>
  sign_bit:
    value: '1 << (7 * len - 1)'
  value_signed:
    value: '(value ^ sign_bit) - sign_bit'
