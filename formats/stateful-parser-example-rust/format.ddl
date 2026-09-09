{-
This is the Rust variant of the stateful parser example.  In addition to
accessing application-specific state, it passes the parser-defined Packet type
to an external Rust function.

The initial special byte is `A`, set by the application.
-}

-- Process a sequence of commands, storing the result in `out`.
def Packets = build (many (out = builder) (Command out))

-- Parse a "packet", which depends on the application's state.
def Packet =
  block
    let byte = UInt8
    if byte == GetSpecial
      then {| Special = byte |}
      else {| Normal = byte |}

-- Skip spaces
def SkipSpace = @(Many $[' '])

-- Process a top-level protocol command
def Command out =
  block
    SkipSpace

    First

      -- Change the special character, and notify the application
      block
        Match "set"
        SkipSpace
        SetSpecial UInt8
        out

      -- Parse a value, let the application observe it, and add it to the result
      emit out (ObservePacket Packet)


-- Interaction with the application, see `src/native.rs`.
def SetSpecial (x : uint 8) : {}
def GetSpecial : uint 8

-- This external function uses Packet, a type defined by this parser.
def ObservePacket (packet : Packet) : Packet
