{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

module Daedalus.Type.BitData (inferBitData) where

import Data.Foldable (find)
import Control.Monad (zipWithM_, when, unless, forM)
import Data.Maybe (catMaybes, fromJust)

import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Panic
import qualified Daedalus.BDD as BDD
import Daedalus.Pass
import Daedalus.SourceRange

import qualified Daedalus.AST as P
import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Kind

srcTypeToSizedType :: P.SrcType -> TypeM a (Type, BDD.Pat)
srcTypeToSizedType sTy = do
  ty <- checkType KValue sTy
  let badType msg = reportError sTy ("Type " <> backticks (pp ty) <>
                                          " cannot be used as bitdata" <> msg)
  n <- case ty of
         TCon n [] -> do
           m_decl <- lookupTypeDefMaybe n
           case m_decl of
             Nothing -> reportError sTy ("Unknown type " <> pp ty)
             Just decl | Just w <- tctyBD decl -> pure w
             _ -> badType "(type has unknown size)"
         TVar {} -> badType "(unexpected type variable)"
         Type (TUInt (Type (TNum n))) -> pure (BDD.pWild (fromInteger n))
         Type (TSInt (Type (TNum n))) -> pure (BDD.pWild (fromInteger n))
         Type TFloat                  -> pure (BDD.pWild 32)
         Type TDouble                 -> pure (BDD.pWild 64)
         Type (TUInt {}) -> badType "(type has non-constant size)"
         Type (TSInt {}) -> badType "(type has non-constant size)"
         _ -> badType "(type cannot be used in bitdata)"

  pure (ty, n)


bdfSizedType :: P.BitDataField -> TypeM a (Maybe (Type, BDD.Pat))
bdfSizedType bdf =
  case bdf of
    P.BDFLiteral _ m_sty -> traverse srcTypeToSizedType m_sty
    P.BDFField   _ m_sty -> traverse srcTypeToSizedType m_sty
    P.BDFWildcard  m_sty -> traverse srcTypeToSizedType m_sty

-- Sizes of fields n a record.  Also ensure that there is at most
-- one Maybe in the result
fieldSizes ::
  HasRange r =>
  r -> [Located P.BitDataField] -> TypeM a [(Maybe (Type, BDD.Pat))]
fieldSizes r flds =
  do sizes <- mapM (bdfSizedType . thingValue) flds
     let nothings = [ loc | (loc, Nothing) <- zip (map thingRange flds) sizes ]
     case nothings of
       _ : _ : _ ->
        reportDetailedError r "Multiple untyped fields, see: "
                                            [ pp (range x) | x <- nothings ]
       _ -> pure sizes


-- Sizes for each constructor
bodySize ::
  HasRange r => r -> P.BitDataBody -> TypeM a [[(Maybe (Type, BDD.Pat))]]
bodySize r bd =
  case bd of
    P.BitDataUnion ctrs -> mapM (fieldSizes r . snd) ctrs
    P.BitDataStruct fs  -> (:[]) <$> fieldSizes r fs




inferStruct ::
  HasRange r =>
  Name                          {- ^ Name of target type -} ->
  BDD.Width                     {- ^ Total width -} ->
  (r, [ Located P.BitDataField ]) {- ^ Location (for errs) and fields -} ->
  [Maybe (Type, BDD.Pat)]       {- ^ Type and sizes of fields -} ->
  TypeM a TCTyDecl
inferStruct name w (loc, fields) m_sz_tys =
  do sz_tys <- resolveMissingTypes loc w m_sz_tys
     zipWithM_ checkLiteralWidth fields (map snd sz_tys)
     let con = mkFields w (zip fields sz_tys)
         fs  = [ (l, t) | BDData l t <- map bdFieldType (bdFields con) ]

     pure TCTyDecl { tctyName    = TCTy name
                   , tctyParams  = []
                   , tctyBD      = Just (bdPat con)
                   , tctyDef     = TCTyStruct (Just con) fs
                   }

mkFields :: BDD.Width -> [(Located P.BitDataField, (Type, BDD.Pat))] -> BDCon
mkFields w = go [] (BDD.pWild 0) w
  where
  go fs pat todo fields =
    case fields of
      [] -> if todo == 0 then BDCon { bdPat = pat, bdFields = reverse fs }
                         else panic "Saw non-zero remainder" [showPP todo]
      (fld, (ty, fpat)) : rest -> go (f' : fs) (BDD.pSplit pat pat') todo' rest
        where
        n     = BDD.width fpat
        todo' = todo - n
        (f', pat') =
          let (shape,p) =
                case thingValue fld of
                  P.BDFField l _   -> (BDData l ty, fpat)
                  P.BDFLiteral l _ -> (BDTag l, BDD.pInt n l)
                  P.BDFWildcard {} -> (BDWild, BDD.pWild n)
          in ( BDField
                 { bdOffset = todo'
                 , bdWidth  = n
                 , bdFieldType = shape
                 }
             , p
             )




resolveMissingTypes ::
  HasRange r =>
  r -> BDD.Width -> [Maybe (Type, BDD.Pat)] -> TypeM a [(Type, BDD.Pat)]
resolveMissingTypes l w fields =
  do let knownWidths = sum (map (BDD.width . snd) (catMaybes fields))
     when (w < knownWidths) $
       reportDetailedError l
           "Cannot instantiate missing type as type is too wide"
              [ "Type width:" <+> pp w
              , "Width of known fields:" <+> pp knownWidths
              ]

     pure (map (resolveField (w - knownWidths)) fields)

resolveField :: BDD.Width -> Maybe (Type, BDD.Pat) -> (Type, BDD.Pat)
resolveField _w (Just x) = x
resolveField w' Nothing  = ( tUInt (tNum (fromIntegral w'))
                           , BDD.pWild w'
                           )





checkLiteralWidth :: Located P.BitDataField -> BDD.Pat -> TypeM a ()
checkLiteralWidth v@(thingValue -> P.BDFLiteral l _) pat =
  do let n = BDD.width pat
     unless (l < 2 ^ n) $
        reportError v ("Literal value" <+> backticks (pp l) <+>
                              "does not fit into type width of" <+> pp n)
checkLiteralWidth _ _ = pure ()





inferBitData :: P.BitData -> MTypeM (Map TCTyName TCTyDecl)
inferBitData bd = runSTypeM . runTypeM (P.bdName bd) $ do
  m_sz_tyss <- bodySize (P.bdName bd) (P.bdBody bd)

  -- Get the first known type width, if it exists (error otherwise)
  let conNames = case P.bdBody bd of
                   P.BitDataUnion cs  -> map (Just . thingValue . fst) cs
                   P.BitDataStruct {} -> repeat Nothing
  let knownWidths = catMaybes (zipWith knownWidth conNames m_sz_tyss)
  (mbCon,width) <-
     case knownWidths of
       []    -> reportError bd "Unable to determine size of type"
       r : _ -> pure r

  case P.bdBody bd of

    P.BitDataStruct fields ->
      do d <- inferStruct (P.bdName bd) width
                          (P.bdName bd, fields) (head m_sz_tyss)
         pure (Map.singleton (tctyName d) d)

    P.BitDataUnion cons ->

      -- Check all known widths are the same
      do case find ((/= width) . snd) knownWidths of
           Nothing         -> pure ()
           Just (fld', w') ->
             reportDetailedError bd "Mismatched constructor widths"
               [ "constructor" <+> pp (fromJust mbCon) <+> "has width" <+>
                                                                      pp width
               , "constructor" <+> pp (fromJust fld') <+> "has width" <+> pp w'
               ]

         decls <- forM (zip cons m_sz_tyss) \(ctor,sz) ->
                    do let mkIdent ident = ident <> "_" <> thingValue (fst ctor)
                       nm <- deriveNameWith mkIdent (P.bdName bd)
                       inferStruct nm width ctor sz

         let labels = map fst cons
             pats   = map (fromJust . tctyBD) decls
             tags   = map thingValue labels
                      `zip` [ (TCon (tctyName d) [], bdPat <$> tag)
                            | d <- decls
                            , let TCTyStruct tag _ = tctyDef d
                            ]

         -- Check that all constructors are distinct
         let overlap a b = not (BDD.willAlwaysFail (BDD.pAnd a b))
             overlappingCons xs =
               case xs of
                 (l,a) : bs -> [ (l,m) | (m,b) <- bs, overlap a b ] ++
                               overlappingCons bs
                 []     -> []
         case overlappingCons (zip labels pats) of
           [] -> pure ()
           (a,b) : _ -> reportDetailedError bd "Overlapping constructors"
                          [ "constructor" <+> pp a, "constructor" <+> pp b ]

         let decl = TCTyDecl { tctyName    = TCTy (P.bdName bd)
                             , tctyParams  = []
                             , tctyBD      = Just (BDD.pOrs width pats)
                             , tctyDef     = TCTyUnion tags
                             }

         -- remove checks from anonymous types, as they are in the
         -- constructor now
         let decls' = [ d { tctyDef = TCTyStruct Nothing fs }
                      | d <- decls, let  TCTyStruct _ fs = tctyDef d
                      ]
         pure (Map.fromList [ (tctyName d, d) | d <- decl : decls' ])
  where
    knownWidth :: Maybe Label -> [ Maybe (Type, BDD.Pat) ] ->
                  Maybe (Maybe Label, BDD.Width)
    knownWidth lab sz_tys =
      do all_sz_tys <- sequenceA sz_tys
         pure (lab, sum (map (BDD.width . snd) all_sz_tys))

