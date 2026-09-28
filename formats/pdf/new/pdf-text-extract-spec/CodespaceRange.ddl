import Daedalus
import PdfValue

--------------------------------------------------------------------------------
-- Source codes

-- A SourceCode is a CMap input key.  Keep its byte width separate from its
-- numeric value so codes such as <01> and <0001> remain distinct.
def SourceCode =
  block
    $$ =
      Between "<" ">"
        (many
          (state = { width = (0 : uint 8), value = (0 : uint 32) })
          block
            state.width < 4 is true
            width = state.width + 1
            value = state.value <# HexByte
        )
    $$.width > 0 is true


--------------------------------------------------------------------------------
-- Codespace trie

-- The empty value indirectly declares the recursive codespace-trie type.
-- Branch maps are keyed by the inclusive start of a byte interval.
def codespaceTrie =
  block
    branches = empty : [uint 8 -> codespaceEdge]

def codespaceEdge (end : uint 8) (child : codespaceTrie) =
  block
    end   = end
    child = child

-- Parse one source code according to the codespace trie.
def ParseSourceCode (trie : codespaceTrie) : SourceCode =
  ParseSourceCodeAt trie 0 0

def ParseSourceCodeAt
  (trie : codespaceTrie)
  (width : uint 8)
  (value : uint 32) : SourceCode =
  block
    let byte = UInt8
    case lookupLE byte trie.branches of
      nothing ->
        Fail "Invalid CMap character code"

      just branch ->
        block
          let edge = branch.1
          byte <= edge.end is true
          let nextWidth = width + 1
          let nextValue = value <# byte
          if edge.child.branches == empty
            then { width = nextWidth, value = nextValue }
            else ParseSourceCodeAt edge.child nextWidth nextValue

-- Insert a codespace range into the interval trie.
-- Fails if codespace ranges have overlapping prefixes.
def InsertCodespace
  (start : SourceCode)       -- Inclusive lower bound.
  (end : SourceCode)         -- Inclusive upper bound.
  (trie : codespaceTrie)     -- Trie being extended.
  : codespaceTrie =
  block
    start.width == end.width is true
    let ?sourceStart = start
    let ?sourceEnd = end
    branches = InsertCodespaceBranches 0 trie.branches


-- Extract a byte from the most-significant (left) end; depth 0 is the first
-- byte written in the source code.
def sourceCodeByte (code : SourceCode) (depth : uint 8) : uint 8 =
  block
    let shift = 8 * (code.width - depth - 1 as uint 64)
    (code.value >> shift) as! uint 8


def InsertCodespaceAt (depth: uint 8) (trie : codespaceTrie): codespaceTrie =
  block
    depth < ?sourceStart.width && trie.branches != empty is true
    branches = InsertCodespaceBranches depth trie.branches


-- Construct the untouched remainder of a newly inserted range.  An empty
-- node at the end of the path represents a complete source code.
def NewCodespacePath (depth : uint 8) : codespaceTrie =
  if depth == ?sourceStart.width
    then codespaceTrie
    else
      block
        let low = sourceCodeByte ?sourceStart depth
        let high = sourceCodeByte ?sourceEnd depth
        low <= high is true
        let child = NewCodespacePath (depth + 1)
        branches = insert low (codespaceEdge high child) empty

def InsertCodespaceBranches (depth : uint 8) branches =
  block
    let low  = sourceCodeByte ?sourceStart depth
    let high = sourceCodeByte ?sourceEnd   depth
    low <= high is true
    let result =
      for (state = { branches = empty, todo = low as uint 16 }; branchStart, edge in branches)
        InsertCodespaceBranch (depth + 1) low high branchStart edge state

    -- Insert a new interval at the end
    if result.todo <= (high as uint 16)
      then
        block
          let child = NewCodespacePath (depth + 1)
          insert
            (result.todo as! uint 8)
            (codespaceEdge high child)
            result.branches
      else result.branches

-- Merge one existing branch with the new byte interval while rebuilding the
-- node's disjoint interval map.
def InsertCodespaceBranch
  (depth : uint 8)           -- Next byte position to insert.
  (low : uint 8)             -- First byte in the new interval.
  (high : uint 8)            -- Last byte in the new interval.
  (branchStart : uint 8)     -- First byte in the existing interval.
  (edge : codespaceEdge)     -- Existing interval and child node.
  state                      -- Rebuilt branches and first byte left to do.
  =
  block
    if edge.end < low || high < branchStart
      -- The existing and new intervals are disjoint.
      then
        block
          branches = insert branchStart edge state.branches
          todo     = state.todo
      -- The intervals overlap and the existing edge needs partitioning.
      else
        block
          let overlapStart = if low > branchStart then low  else branchStart
          let overlapEnd   = if high < edge.end   then high else edge.end

          -- Preserve the part of the existing edge before the overlap.
          let withLeft =
            if branchStart < overlapStart
              then
                insert
                  branchStart
                  (codespaceEdge (overlapStart - 1) edge.child)
                  state.branches
              else
                state.branches

          -- Fill any new interval not covered before this existing edge.
          let withGap =
            if state.todo < (overlapStart as uint 16)
              then
                insert
                  (state.todo as! uint 8)
                  (codespaceEdge (overlapStart - 1) (NewCodespacePath depth))
                  withLeft
              else
                withLeft

          -- Merge the common interval into the existing child trie.
          let withOverlap =
            insert
              overlapStart
              (codespaceEdge overlapEnd (InsertCodespaceAt depth edge.child))
              withGap

          branches =
            -- Preserve the part of the existing edge after the overlap.
            if overlapEnd < edge.end
              then insert (overlapEnd + 1) edge withOverlap
              else withOverlap
          todo =
            block
              let afterOverlap = (overlapEnd as uint 16) + 1
              if state.todo > afterOverlap
                then state.todo
                else afterOverlap


--------------------------------------------------------------------------------
-- Hexadecimal bytes

def HexD =
  First
    $['0' .. '9'] - '0'
    10 + $['a' .. 'f'] - 'a'
    10 + $['A' .. 'F'] - 'A'

def HexByte = 16 * HexD + HexD
