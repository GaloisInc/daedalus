

def Main =
    -- "Hello World!"       -- Match the input string "Hello World!"
    -- SimpleStruct      -- Match hello world as two fields
    -- SimpleUnion       -- Match either "Hello" or "World"
    -- Digit             -- Match a single digit, return as an int
    -- ManyDigit         -- Match multiple digits, returning a list of digits
    -- BigNumber         -- Match a number greater than 100, return that number
    TokenNumber       -- Match a number with some whitespace

def SimpleStruct = {
    first = "Hello";
    second = "World";
}

def SimpleUnion = Choose {
    hello = "Hello";
    world = "World";
}

def Digit = {
    @v = '0' .. '9';
    ^ (v - '0' as int);
}

def ManyDigit = Many (1..) Digit

def BigNumber = {
    $$ = Natural;     -- $$ is the 'default' semantic value of the rule
    $$ > 100;
}
-- Convert a list of digits to a natural
def Natural = {
  @ds = ManyDigit;
  ^ for (val = 0; d in ds) (addDigit val d);
}
-- Combine a single digit with the accumulator
def addDigit val d = val * 10 + d

def TokenNumber = Token BigNumber
-- A Token allows for whitespace after the parser in the argument
def Token P = { $$ = P; $ws }    
-- A character class containing spaces and newlines
def $ws = ' ' | '\n'
