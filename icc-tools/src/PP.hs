{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
module PP where

import GHC.Records (getField)
import Text.PrettyPrint hiding ((<>))
import qualified RTS.Input as RTS
import qualified RTS.Vector as RTS
import qualified RTS.Numeric as RTS
import ICC


class PP a where
  pp :: a -> Doc

instance (RTS.VecElem a, PP a) => PP (RTS.Vector a) where
  pp xs = vcat (zipWith ppEl [0..] (RTS.toList xs))
    where ppEl x y = brackets (int x) <+> pp y

instance RTS.SizeType n => PP (RTS.UInt n) where
  pp x = integer (RTS.asInt x)

instance PP Main where
  pp x = pp (getField @"tags" x)


ppTag :: Doc -> Doc -> Doc
ppTag x y = hang (x <> colon) 2 y

instance PP Tag where
  pp tag =
    case tag of
      Tag_desc xs -> ppTag "desc" (pp xs)
      Tag_A2B0 xs -> ppTag "A2B0" (pp xs)
      Tag_A2B1 xs -> ppTag "A2B1" (pp xs)
      Tag_A2B2 xs -> ppTag "A2B2" (pp xs)
      Tag_A2B3 xs -> ppTag "A2B3" (pp xs)
      Tag_A2M0 xs -> ppTag "A2M0" (pp xs)

      Tag_B2A0 xs -> ppTag "B2A0" (pp xs)
      Tag_B2A1 xs -> ppTag "B2A1" (pp xs)
      Tag_B2A2 xs -> ppTag "B2A2" (pp xs)
      Tag_B2A3 xs -> ppTag "B2A3" (pp xs)

      Tag_B2D0 xs -> ppTag "B2D0" (pp xs)
      Tag_B2D1 xs -> ppTag "B2D1" (pp xs)
      Tag_B2D2 xs -> ppTag "B2D2" (pp xs)
      Tag_B2D3 xs -> ppTag "B2D3" (pp xs)

      Tag_wtpt xs -> ppTag "wtpt" (pp xs)
      Tag_cprt xs -> ppTag "cprt" (pp xs)
      Tag_c2sp xs -> ppTag "c2sp" (pp xs)
      Tag_s2cp xs -> ppTag "s2cp" (pp xs)

      Tag_svcn xs -> ppTag "svcn" (pp xs)

      Tag_unknown {} -> text (show tag)

instance PP LutAB_or_multi where
  pp a =
    case a of
      LutAB_or_multi_lutAB x -> pp x
      LutAB_or_multi_mpe x -> pp x

instance PP LutBA_or_multi where
  pp a =
    case a of
      LutBA_or_multi_lutBA x -> pp x
      LutBA_or_multi_mpe x  -> pp x


instance PP MultiProcessElementsType where
  pp x = hang header 2 els
    where
    header = "MPE" <+> parens (hsep (punctuate comma [ins,outs]))
    ins    = "inputs:"  <+> pp (getField @"number_of_input_channels" x)
    outs   = "outputs:" <+> pp (getField @"number_of_output_channels" x)
    els    = pp (getField @"elements" x)

instance PP MPElement where
  pp x = pp (getField @"head" x) $$ pp (getField @"body" x)

instance PP MPElementHead where
  pp x = parens (hsep (punctuate comma [ins,outs]))
    where
    ins    = "inputs:"  <+> pp (getField @"inputs" x)
    outs   = "outputs:" <+> pp (getField @"outputs" x)


instance PP MPElementBody where
  pp x =
    case x of
      MPElementBody_calc x -> pp x
      _ -> text (show x) -- XXX


instance PP CalcElement where
  pp x = vcat [ "main =" $$ nest 2 (pp (getField @"main" x))
              , "subelements:" $$ nest 2 (pp (getField @"subElements" x))
              ]

instance PP FunOpWithPosition where
  pp x = brackets (int (RTS.inputOffset (getField @"offset" x))) <+>
                                              (pp (getField @"op" x))

params :: OpParams -> [Doc]
params x = [ pp (getField @"s" x), pp (getField @"t" x) ]

ppOp :: Doc -> [Doc] -> Doc
ppOp x ys = x <> parens (hsep (punctuate comma ys))

instance PP FunOp where
  pp op =
    case op of
      FunOp_data x    -> float x
      FunOp_opIn x    -> ppOp "in" (params x)
      FunOp_opOut x   -> ppOp "out" (params x)
      FunOp_opTGet x  -> ppOp "tget" (params x)
      FunOp_opTPut x  -> ppOp "tput" (params x)
      FunOp_opTSave x -> ppOp "tsav" (params x)
      FunOp_opEnv x   -> ppOp "env" [ pp x ]

      FunOp_curv x -> ppOp "curv" [ pp x ]
      FunOp_mtx x  -> ppOp "mtx" [ pp x ]
      FunOp_calc x -> ppOp "calc" [ pp x ]
      FunOp_tint x -> ppOp "tint" [ pp x ]
      FunOp_elem x -> ppOp "elem" [ pp x ]

      FunOp_copy x -> ppOp "copy" (params x)
      FunOp_rotl x -> ppOp "rotl" (params x)
      FunOp_rotr x -> ppOp "rotr" (params x)
      FunOp_posd x -> ppOp "posd" (params x)
      FunOp_flip x -> ppOp "flip" [pp x]
      FunOp_pop x  -> ppOp "pop" [pp x]

      FunOp_solv x  -> ppOp "solv" (params x)
      FunOp_tran x  -> ppOp "tran" (params x)

      FunOp_sum x   -> ppOp "sum" [pp x]
      FunOp_prod x  -> ppOp "prod" [pp x]
      FunOp_min x   -> ppOp "min" [pp x]
      FunOp_max x   -> ppOp "max" [pp x]
      FunOp_and x   -> ppOp "and" [pp x]
      FunOp_or x    -> ppOp "or" [pp x]

      FunOp_opIfThen x ->
        vcat [ "if", nest 2 (pp x) ]

      FunOp_opIfThenElse x ->
        vcat [ "if", nest 2 (pp (getField @"thenOps" x))
             , "else", nest 2 (pp (getField @"elseOps" x))
             ]

      FunOp_opPi     x -> ppOp "pi" [pp x]
      FunOp_opPosInf x -> ppOp "+inf" [pp x]
      FunOp_opNegInf x -> ppOp "-inf" [pp x]
      FunOp_opNAN    x -> ppOp "nan" [pp x]
      FunOp_opAdd    x -> ppOp "add" [pp x]
      FunOp_opSub    x -> ppOp "sub" [pp x]
      FunOp_opMul    x -> ppOp "mul" [pp x]
      FunOp_opDiv    x -> ppOp "div" [pp x]
      FunOp_opMod    x -> ppOp "mod" [pp x]
      FunOp_opPow    x -> ppOp "pow" [pp x]
      FunOp_opGamma  x -> ppOp "gamma" [pp x]
      FunOp_opSAdd   x -> ppOp "sadd" [pp x]
      FunOp_opSSub   x -> ppOp "ssub" [pp x]
      FunOp_opSMul   x -> ppOp "smul" [pp x]
      FunOp_opSDiv   x -> ppOp "sdiv" [pp x]
      FunOp_opSq     x -> ppOp "sq" [pp x]
      FunOp_opSqrt   x -> ppOp "sqrt" [pp x]
      FunOp_opCb     x -> ppOp "cb" [pp x]
      FunOp_opCbrt   x -> ppOp "cbrt" [pp x]
      FunOp_opAbs    x -> ppOp "abs" [pp x]
      FunOp_opNeg    x -> ppOp "neg" [pp x]
      FunOp_opRond   x -> ppOp "rond" [pp x]
      FunOp_opFlor   x -> ppOp "flor" [pp x]
      FunOp_opCeil   x -> ppOp "ceil" [pp x]
      FunOp_opTrnc   x -> ppOp "trnc" [pp x]
      FunOp_opSign   x -> ppOp "sign" [pp x]
      FunOp_opExp    x -> ppOp "exp" [pp x]
      FunOp_opLog    x -> ppOp "log" [pp x]
      FunOp_opLn     x -> ppOp "ln" [pp x]
      FunOp_opSin    x -> ppOp "sin" [pp x]
      FunOp_opCos    x -> ppOp "cos" [pp x]
      FunOp_opTan    x -> ppOp "tan" [pp x]
      FunOp_opASin   x -> ppOp "asin" [pp x]
      FunOp_opACos   x -> ppOp "acos" [pp x]
      FunOp_opATan   x -> ppOp "atan" [pp x]
      FunOp_opATn2   x -> ppOp "atn2" [pp x]
      FunOp_opCTop   x -> ppOp "ctop" [pp x]
      FunOp_opPToc   x -> ppOp "ptoc" [pp x]
      FunOp_opRNum   x -> ppOp "rnum" [pp x]
      FunOp_opLT     x -> ppOp "lt" [pp x]
      FunOp_opLE     x -> ppOp "le" [pp x]
      FunOp_opEQ     x -> ppOp "eq" [pp x]
      FunOp_opNear   x -> ppOp "near" [pp x]
      FunOp_opGE     x -> ppOp "ge" [pp x]
      FunOp_opGT     x -> ppOp "gt" [pp x]
      FunOp_opVMin   x -> ppOp "vmin" [pp x]
      FunOp_opVMax   x -> ppOp "vmax" [pp x]
      FunOp_opVAnd   x -> ppOp "vand" [pp x]
      FunOp_opVOr    x -> ppOp "vor" [pp x]
      FunOp_opTLab   x -> ppOp "tlab" [pp x]
      FunOp_opTXYZ   x -> ppOp "txyz" [pp x]


{-
      -- Table 105
      0s"sel " ->
        block
          NoParams
          let c1 = SelCase
          let cs = Many SelCase
          let d  = Optional   block Match "dflt"; BE32 as uint 64
          {| opSel = Sel c1 cs d |}
-}

      _ -> text (show op) -- XXX

--------------------------------------------------------------------------------
-- XXX


instance PP LutAToBType where
  pp x = text (show x) -- XXX

instance PP LutBToAType where
  pp x = text (show x) -- XXX

instance PP UnicodeRecord where
  pp = text . show

instance PP XYZNumber where
  pp = text . show

instance PP SpectralViewingConditionsType where
  pp = text . show


