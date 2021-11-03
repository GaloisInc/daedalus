{-# Language OverloadedStrings, TypeApplications, DataKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language AllowAmbiguousTypes, KindSignatures #-}
module PP where

import GHC.Records (HasField,getField)
import GHC.TypeLits(Symbol,symbolVal,KnownSymbol)
import Text.PrettyPrint hiding ((<>))
import Numeric (showHex)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as BS8
import Data.Proxy(Proxy(Proxy))
import qualified RTS.ParserAPI as RTS
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

instance RTS.SizeType n => PP (RTS.SInt n) where
  pp x = integer (RTS.asInt x)


instance PP Text where
  pp = text . Text.unpack

instance PP BS8.ByteString where
  pp = text . BS8.unpack

instance PP Float where
  pp = float

instance PP a => PP (Maybe a) where
  pp x = case x of
           Nothing -> "(nothing)"
           Just a  -> pp a

--------------------------------------------------------------------------------

instance PP RTS.ParseError where
  pp x =
    vcat [ "ERROR at offset" <+> (loc <> colon)
         , nest 2 $ vcat [ text (RTS.peMsg x)
                         , "-- stack ---"
                         , vcat (map text (RTS.peStack x))
                         ]
         ]
    where
    off = RTS.inputOffset (RTS.peInput x)
    loc = int off <+> brackets ("0x" <> text (showHex off ""))


--------------------------------------------------------------------------------

instance PP Main where
  pp x = vcat [ ppF @"header" x
              , ppF @"tags" x

              ]

instance PP ProfileHeader where
  pp h =
    vcat [ ppF' @"size" h
         , "preferred_cmm_type:" <+> ppBytes (getField @"preferred_cmm_type" h)
         , ppF' @"version" h
         ]

{-
         , "devce_class" <+> ppF @"display_device_profile
    color_space         = DataColorSpace
    pcs                 = DataColorSpace  -- check additional constraints?
    creation_date_time  = DateTimeNumber
    Match "acsp"
    primary_platform    = PrimaryPlatform
    profile_flags       = BE32            -- XXX: bitdata, see 7.2.13
    device_manufacturer = Many 4 UInt8
    device_model        = Many 4 UInt8
    device_attributes   = BE64            -- XXX: bitdata, see 7.2.16
    rendering_intent    = RenderingIntent
    illuminant          = XYZNumber
    creator             = Many 4 UInt8
    identifier          = Many 16 UInt8
    reserved_data       = Many 28 UInt8      -- XXX: spectral pcc
-}

instance PP VersionField where
  pp v = ppF @"major" v <> "." <> ppF @"minor" v <> "." <> ppF @"bugfix" v

--------------------------------------------------------------------------------
ppBytes :: RTS.Vector (RTS.UInt 8) -> Doc
ppBytes = pp . RTS.vecToRep

ppBytes7 :: RTS.Vector (RTS.UInt 7) -> Doc
ppBytes7 = pp . RTS.vecToRep

ppBlock :: (RTS.VecElem a, PP a) => RTS.Vector a -> Doc
ppBlock vec = vcat [ ppRow i rowLen | i <- take (fromIntegral rows) [ 0..]]
           $$ ppRow rowLen extra
  where
  rowLen = 8
  (rows,extra) = divMod (RTS.fromUInt (RTS.length vec)) rowLen

  ppRow i len =
    hsep [ ppEl x | x <- take (fromIntegral len) [ i * rowLen .. ] ]
  ppEl i = case vec RTS.!? RTS.UInt i of
             Just a  -> pp a
             Nothing -> empty

ppTag :: Doc -> Doc -> Doc
ppTag x y = hang (x <> colon) 2 y

ppF :: forall (x :: Symbol) r a. (HasField x r a, PP a) => r -> Doc
ppF r = pp (getField @x r)

ppF' ::
  forall (x :: Symbol) r a. (HasField x r a, PP a, KnownSymbol x) => r -> Doc
ppF' r = hang (text (symbolVal (Proxy :: Proxy x)) <> ":") 2 (ppF @x r)
--------------------------------------------------------------------------------


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

      Tag_rXYZ xs -> ppTag "rXYZ" (pp xs)
      Tag_gXYZ xs -> ppTag "gXYZ" (pp xs)
      Tag_bXYZ xs -> ppTag "bXYZ" (pp xs)

      Tag_rTRC xs -> ppTag "rTRC" (pp xs)
      Tag_gTRC xs -> ppTag "gTRC" (pp xs)
      Tag_bTRC xs -> ppTag "bTRC" (pp xs)

      Tag_dmdd x  -> ppTag "dmdd" (pp x)
      Tag_dmnd x  -> ppTag "dmnd" (pp x)

      Tag_chrm x  -> ppTag "chrm" (pp x)
      Tag_chad x  -> ppTag "chad" (pp x)

      Tag_unimplemented {} -> text (show tag)
      Tag_invalid_tag x -> pp x

instance PP InvalidTag where
  pp i =
    vcat [ "INVALID" <+> ppBytes (getField @"sig" i)
         , nest 2 (ppBytes (getField @"data" i))
         ]


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

instance PP CurveOrPCurve where
  pp a =
    case a of
      CurveOrPCurve_curve x -> ppBlock x
      CurveOrPCurve_pcurve x -> pp x

--------------------------------------------------------------------------------

instance PP MPElement where
  pp x = ppF @"head" x $$ ppF @"body" x

instance PP MPElementHead where
  pp x = parens (hsep (punctuate comma [ins,outs]))
    where
    ins    = "inputs:"  <+> ppF @"inputs" x
    outs   = "outputs:" <+> ppF @"outputs" x


instance PP MPElementBody where
  pp a =
    case a of
      MPElementBody_calc x -> pp x
      MPElementBody_cvst x -> pp x
      MPElementBody_matf x -> pp x
      MPElementBody_mpet x -> pp x
      MPElementBody_unimplemented x -> text (show x)

instance PP Curve where
  pp c =
    case c of
      Curve_sngf x -> pp x
      Curve_curf x -> pp x
      Curve_unimplemented x -> text (show x)

instance PP SingleSampledCurve where
  pp x = braces $ hsep $ punctuate comma
          [ ppF' @"n" x
          , ppF' @"f" x
          , ppF' @"l" x
          , ppF' @"e" x
          , ppF' @"ty" x
          ]

instance PP SegmentedCurve where
  pp x = vcat [ ppF' @"breakPoints" x
              , ppF' @"segments" x
              ]

instance PP CurveSegment where
  pp a =
    case a of
      CurveSegment_parf x -> pp x
      CurveSegment_samf x -> pp x

instance PP FormualCurveSegment where
  pp s = "F" <> ppF @"fun" s <> parens (hsep (punctuate comma (map pp as)))
    where as = RTS.toList (getField @"args" s)

instance PP Matrix where
  pp m = vcat [ ppF' @"matrix" m, ppF' @"vector" m ]



instance PP CalcElement where
  pp x = vcat [ "main =" $$ nest 2 (ppF @"main" x)
              , "subelements:" $$ nest 2 (ppF @"subElements" x)
              ]

instance PP FunOpWithPosition where
  pp x = brackets (int (RTS.inputOffset (getField @"offset" x))) <+> ppF @"op" x

params :: OpParams -> [Doc]
params x = [ ppF @"s" x, ppF @"t" x ]

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
        vcat [ "if", nest 2 (ppF @"thenOps" x)
             , "else", nest 2 (ppF @"elseOps" x)
             ]

      FunOp_opSel x ->
        "sel" $$ nest 2 (vcat $
                          ppCase (getField @"case1" x)
                        : map ppCase (RTS.toList (getField @"cases" x))
                       ++ ppDflt)
        where ppCase c = "case" $$ nest 2 (pp c)
              ppDflt   = case getField @"dflt" x of
                           Nothing -> []
                           Just y  -> ["dflt" $$ nest 2 (pp y)]

      FunOp_opPi     {} -> "pi"
      FunOp_opPosInf {} -> "+inf"
      FunOp_opNegInf {} -> "-inf"
      FunOp_opNaN    {} -> "nan"
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

instance PP XYZNumber where
  pp xyz = braces (hsep (punctuate comma
                            [ ppF' @"x" xyz
                            , ppF' @"y" xyz
                            , ppF' @"z" xyz
                            ]) )

instance PP XYNumber where
  pp xyz = braces (hsep (punctuate comma
                            [ ppF' @"x" xyz
                            , ppF' @"y" xyz
                            ]) )




instance PP UnicodeRecord where
  pp u = braces (lang <+> "|" <+> country <+> "|" <+> txt)
    where lang    = pp (RTS.vecToRep (getField @"language" u))
          country = pp (RTS.vecToRep (getField @"country"  u))
          txt     = pp (Text.decodeUtf16BE (RTS.vecToRep (getField @"data" u)))

instance PP LaxTextType where
  pp u =
    case u of
      LaxTextType_uni x -> pp x
      LaxTextType_desc x -> pp x
      LaxTextType_text x -> ppBytes7 x

instance PP TextDescriptionType where
  pp x =
    braces $
    vcat [ opt a $ "ascii:" <+> ppBytes a
         , opt u (("unicode" <> parens (ppBytes uc) <> colon) <+>
                                pp (Text.decodeUtf16BE (RTS.vecToRep u)))
         , opt s (("script" <> parens (ppBytes sc) <> colon) <+>
                                                      ppBytes s)
         ]
    where
    opt v f = if RTS.sizeToInt (RTS.length v) == 0 then empty else f

    a   = getField @"ascii_data"   x
    uc  = getField @"unicode_code" x
    u   = getField @"unicode_data" x
    sc  = getField @"script_code"  x
    s   = getField @"script_data"  x


instance PP ParametricCurveType where
  pp c = hcat [ "F", ppF @"function" c, parens (hsep (punctuate comma as)) ]
    where as = map pp (RTS.toList (getField @"parameters" c))

instance PP ChromaticityType where
  pp c = braces $ hsep $ punctuate comma
              [ ppF' @"phosphor_or_colorant" c, ppF' @"cie_coords" c ]

--------------------------------------------------------------------------------
-- XXX


instance PP LutAToBType where
  pp x = text (show x) -- XXX

instance PP LutBToAType where
  pp x = text (show x) -- XXX



instance PP SpectralViewingConditionsType where
  pp = text . show


