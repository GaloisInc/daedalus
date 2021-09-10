{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}

module Daedalus.Type.BitData (inferBitData) where

import Data.Foldable (find)
import Control.Monad (zipWithM_, when, unless, forM)
import Data.Maybe (catMaybes, fromJust)
import Data.Bits  (shiftL, (.|.) )

import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Panic
import qualified Daedalus.BDD as BDD
import Daedalus.Pass
import Daedalus.SourceRange

import Daedalus.AST
import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Kind

srcTypeToSizedType :: SrcType -> TypeM a (Type, BDD.Pat)
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
         Type (TUInt {}) -> badType "(type has non-constant size)"
         Type (TSInt {}) -> badType "(type has non-constant size)"
         _ -> badType "(type cannot be used in bitdata)"

  pure (ty, n)


bdfSizedType :: BitDataField -> TypeM a (Maybe (Type, BDD.Pat))
bdfSizedType bdf =
  case bdf of
    BDFLiteral _ m_sty -> traverse srcTypeToSizedType m_sty
    BDFField   _ m_sty -> traverse srcTypeToSizedType m_sty
    BDFWildcard  m_sty -> traverse srcTypeToSizedType m_sty

-- Sizes of fields n a record.  Also ensure that there is at most
-- one Maybe in the result
fieldSizes ::
  HasRange r => r -> [Located BitDataField] -> TypeM a [(Maybe (Type, BDD.Pat))]
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
  HasRange r => r -> BitDataBody -> TypeM a [[(Maybe (Type, BDD.Pat))]]
bodySize r bd =
  case bd of
    BitDataUnion ctrs -> mapM (fieldSizes r . snd) ctrs
    BitDataStruct fs  -> (:[]) <$> fieldSizes r fs




inferStruct ::
  HasRange r =>
  Name                          {- ^ Name of target type -} ->
  BDD.Width                     {- ^ Total width -} ->
  (r, [ Located BitDataField ]) {- ^ Location (for errs) and fields -} ->
  [Maybe (Type, BDD.Pat)]       {- ^ Type and sizes of fields -} ->
  TypeM a TCTyDecl
inferStruct name w (loc, fields) m_sz_tys =
  do sz_tys <- resolveMissingTypes loc w m_sz_tys
     let szs = map snd sz_tys

     zipWithM_ checkLiteralWidth fields szs

     let (umeta,cpat) = mkConMeta w (zip fields szs)
         (fs,fpat)    = mkFields w (zip fields sz_tys)
         recognize    = BDD.pAnd cpat fpat

     pure TCTyDecl { tctyName    = TCTy name
                   , tctyParams  = []
                   , tctyBD      = Just recognize
                   , tctyDef     = TCTyStruct (Just umeta) fs
                   }



mkFields ::
  BDD.Width ->
  [(Located BitDataField, (Type, BDD.Pat))] ->
  ([(Label, (Type, Maybe TCBDStructMeta))], BDD.Pat)

mkFields w = go [] (BDD.pWild w) w
  where
  go fs pat todo fields =
    case fields of
      [] -> if todo == 0 then (reverse fs, pat)
                         else panic "Saw non-zero remainder" [showPP todo]
      (fld, (ty, fpat)) : rest -> go fs' pat' todo' rest
        where
        n     = BDD.width fpat
        todo' = todo - n
        (fs',pat') =
           case thingValue fld of
             BDFField l _ ->
               ( (l, (ty, Just TCBDStructMeta { tcbdsLowBit = todo'
                                              , tcbdsWidth  = n }))
                 : fs
               , BDD.pField todo' fpat pat
               )
             _ -> (fs,pat)


-- This assumes the high bits are stored as the first fields
mkConMeta ::
  BDD.Width -> [(Located BitDataField,BDD.Pat)] -> (TCBDUnionMeta, BDD.Pat)
mkConMeta w = go 0 0 w (BDD.pWild w)
  where
  -- XXX: foldl'
  go macc bacc _todo pat [] =
      ( TCBDUnionMeta { tcbduMask = macc, tcbduBits = bacc }
      , pat
      )
  go macc bacc todo pat ((fld, fpat) : rest) =
    let n     = BDD.width fpat
        todo' = todo - n
        off   = fromIntegral todo' :: Int
        num l = (2 ^ n - 1, l, BDD.pField todo' (BDD.pInt n l) pat)
        (mask, v, pat') = case thingValue fld of
                            BDFLiteral l _ -> num l
                            _              -> (0, 0, pat)
    in go (macc .|. (mask `shiftL` off))
          (bacc .|. (v    `shiftL` off))
          todo'
          pat'
          rest



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





checkLiteralWidth :: Located BitDataField -> BDD.Pat -> TypeM a ()
checkLiteralWidth v@(thingValue -> BDFLiteral l _) pat =
  do let n = BDD.width pat
     unless (l < 2 ^ n) $
        reportError v ("Literal value" <+> backticks (pp l) <+>
                              "does not fit into type width of" <+> pp n)
checkLiteralWidth _ _ = pure ()





inferBitData :: BitData -> MTypeM (Map TCTyName TCTyDecl)
inferBitData bd = runSTypeM . runTypeM (bdName bd) $ do
  m_sz_tyss <- bodySize (bdName bd) (bdBody bd)

  -- Get the first known type width, if it exists (error otherwise)
  let conNames = case bdBody bd of
                   BitDataUnion cs  -> map (Just . thingValue . fst) cs
                   BitDataStruct {} -> repeat Nothing
  let knownWidths = catMaybes (zipWith knownWidth conNames m_sz_tyss)
  (mbCon,width) <-
     case knownWidths of
       []    -> reportError bd "Unable to determine size of type"
       r : _ -> pure r

  case bdBody bd of

    BitDataStruct fields ->
      do d <- inferStruct (bdName bd) width (bdName bd, fields) (head m_sz_tyss)
         pure (Map.singleton (tctyName d) d)

    BitDataUnion cons ->

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
                       nm <- deriveNameWith mkIdent (bdName bd)
                       inferStruct nm width ctor sz

         let labels = map fst cons
             pats   = map (fromJust . tctyBD) decls
             tags   = map thingValue labels
                      `zip` [ (TCon (tctyName d) [], tag)
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

         let decl = TCTyDecl { tctyName    = TCTy (bdName bd)
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

