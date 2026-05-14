{-
This file demonstrate a parser which utilizes some application specific
state while parsing.  The format processes a sequence of "commands", which result
in a sequence of packets.  A "command" will either modify the application's
state or parse a "packet".  A "packet" is just a byte, but it is processed
differently if it matches the application "special" byte.

The initial special byte is `A` set in the application (see `main.cpp`)
-}

-- Process a sequence of commands, storing the result in `out`.
def Packets = build (many (out = builder) (Command out))

-- Parse a "packet", which depends on the application's state.
def Packet =
  block
    let byte = UInt8
    if byte == GetSpecial         -- here we access the application's state
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
        SetSpecial UInt8        -- here we modify the application's state
        out
    
      -- Parse a value and add it to the result
      emit out Packet


-- Interaction with the application, see `state.h` for the implementations
def SetSpecial (x : uint 8) : {}
def GetSpecial : uint 8


