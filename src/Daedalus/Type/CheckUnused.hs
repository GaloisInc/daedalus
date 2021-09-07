{-# Language GADTs, NamedFieldPuns #-}
module Daedalus.Type.CheckUnused where

import Data.Foldable(toList)
import Data.Maybe(isNothing)

import Daedalus.Rec

import Daedalus.Type.AST

-- import Debug.Trace
-- import Daedalus.PP

checkTCModule :: TCModule SourceRange -> [SourceRange]
checkTCModule = concatMap checkTCDecl . forgetRecs . tcModuleDecls

checkTCDecl :: TCDecl SourceRange -> [SourceRange]
checkTCDecl TCDecl { tcDeclCtxt, tcDeclDef } =
  case tcDeclCtxt of
    AGrammar | Defined e <- tcDeclDef -> checkTC False e
    _ -> []

checkTC :: Bool -> TC SourceRange Grammar -> [SourceRange]
checkTC triv tc =
  --  | trace (show (triv, pp tc)) False = undefined
  --  | otherwise =
  let warnIf b = if b then [ texprAnnot tc ] else []
      okLeaf   = []
  in
  case texprValue tc of
    TCPure e -> case typeOf e of
                  Type TUnit -> okLeaf
                  _          -> warnIf triv

    TCDo mb e1 e2 ->
      checkTC (isNothing mb) e1 ++ checkTC False e2

    TCLabel _ g -> checkTC triv g

    TCGetByte {}    -> okLeaf
    TCMatch {}      -> okLeaf
    TCMatchBytes {} -> okLeaf

    TCChoice _ gs _ -> concatMap (checkTC False) gs
    TCOptional _ g  -> checkTC True g
    TCMany _ _ _ g  -> checkTC True g

    TCEnd           -> okLeaf
    TCOffset        -> warnIf triv
    TCCurrentStream -> warnIf triv
    TCSetStream {}  -> okLeaf
    TCStreamLen {}  -> okLeaf
    TCStreamOff {}  -> okLeaf

    TCMapLookup {}  -> okLeaf
    TCMapInsert {}  -> okLeaf
    TCArrayIndex {} -> okLeaf
    TCCoerceCheck {} -> okLeaf

    TCFor lp -> checkTC False (loopBody lp)

    TCIf _ th el -> checkTC False th ++ checkTC False el
    TCCall {} -> okLeaf

    TCVar {} -> okLeaf
    TCErrorMode _ g -> checkTC triv g
    TCFail {} -> okLeaf

    -- we don't treat unused results in a case as an error
    TCCase _ as d ->
      concat $  map (checkTC False) (toList d) ++ map checkAlt (toList as)
      where
      checkAlt a = checkTC False (tcAltBody a)


