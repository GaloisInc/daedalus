module Daedalus.ParserGen.LL.ParamLL
  ( cst_CLOSURE_MAX_DEPTH
  , cst_MAX_LOOKAHEAD_DEPTH
  , cst_MAX_DFA_NB_STATES
  , cst_OVERFLOW_CFG
  , cst_DEMO_MODE
  , cst_MAX_LLA_SIZE
  , flag_ONLY_STRICT_LLA
  )
where


-- Closure params
cst_CLOSURE_MAX_DEPTH :: Int
cst_CLOSURE_MAX_DEPTH = 200


-- DFA params
cst_MAX_LOOKAHEAD_DEPTH :: Int
cst_MAX_LOOKAHEAD_DEPTH = 20

cst_MAX_DFA_NB_STATES :: Int
cst_MAX_DFA_NB_STATES = 100

cst_OVERFLOW_CFG :: Int
cst_OVERFLOW_CFG = 10

cst_DEMO_MODE :: Bool
cst_DEMO_MODE = True



-- LLA params
cst_MAX_LLA_SIZE :: Int
cst_MAX_LLA_SIZE = 4000

flag_ONLY_STRICT_LLA :: Bool
flag_ONLY_STRICT_LLA = False
