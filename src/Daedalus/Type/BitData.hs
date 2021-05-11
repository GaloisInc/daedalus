{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Daedalus.Type.BitData (inferBitData) where

import Data.Foldable (find)
import Control.Monad (zipWithM_, zipWithM, when, unless)
import Data.Maybe (catMaybes)
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

inferCtor ::
  Name ->
  BDD.Width ->
  (Located Label, [ Located BitDataField ]) ->
  [Maybe (Type, BDD.Pat)] ->
  TypeM a (TCTyDecl, (Label, (Type, Maybe TCBDUnionMeta)), BDD.Pat)
inferCtor tyN w (ctor, fields) m_sz_tys = do
  sz_tys <- resolveMissingTypes ctor w m_sz_tys
  let (_tys, szs) = unzip sz_tys

  zipWithM_ checkLiteralWidth fields szs

  let (umeta,cpat) = mkUnionMeta w (zip fields szs)
      (fs,fpat) = mkFields w (zip fields sz_tys)

  n' <- TCTy <$> deriveNameWith mkIdent tyN
  let decl = TCTyDecl { tctyName    = n'
                      , tctyParams  = []
                      , tctyBD      = Just fpat
                      , tctyDef     = TCTyStruct fs
                      }
      ty   = TCon n' []

  pure (decl, (thingValue ctor, (ty, Just umeta)), BDD.pAnd cpat fpat)

  where
    mkIdent ident = ident <> "_" <> thingValue ctor



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
mkUnionMeta ::
  BDD.Width -> [(Located BitDataField,BDD.Pat)] -> (TCBDUnionMeta, BDD.Pat)
mkUnionMeta w = go 0 0 w (BDD.pWild w)
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
  m_sz_tyss <- mapM (mapM (bdfSizedType . thingValue) . snd ) (bdCtors bd)

  -- Check that no rule has more than 1 underspecified width
  zipWithM_ checkUnderspec (bdCtors bd) m_sz_tyss

  -- Get the first known type width, if it exists (error otherwise)
  let knownWidths = catMaybes (zipWith knownWidth (bdCtors bd) m_sz_tyss)
  (kfld, width) <- case knownWidths of
    []            -> reportError bd "Unable to determine size of type"
    r : _         -> pure r

  -- Check all known widths are the same
  case find (not . (==) width . snd) knownWidths of
    Nothing         -> pure ()
    Just (fld', w') -> reportDetailedError bd "Mismatched constructor widths"
      [ "constructor" <+> pp kfld <+> "has width" <+> pp width
      , "constructor" <+> pp fld' <+> "has width" <+> pp w'
      ]

  (decls, ctors, pats) <-
      unzip3 <$> zipWithM (inferCtor (bdName bd) width)
                                              (bdCtors bd) m_sz_tyss

  -- Check that all constructors are distinct
  let overlap a b = not (BDD.willAlwaysFail (BDD.pAnd a b))
      overlappingCons xs =
        case xs of
          (l,a) : bs -> [ (l,m) | (m,b) <- bs, overlap a b ] ++
                        overlappingCons bs
          []     -> []
  case overlappingCons (zip (map fst ctors) pats) of
    [] -> pure ()
    (a,b) : _ -> reportDetailedError bd "Overlapping constructors"
                   [ "constructor" <+> pp a, "constructor" <+> pp b ]

  let decl = TCTyDecl { tctyName    = TCTy (bdName bd)
                      , tctyParams  = []
                      , tctyBD      = Just (BDD.pOrs width pats)
                      , tctyDef     = TCTyUnion ctors
                      }

  pure (Map.fromList [ (tctyName d, d) | d <- decl : decls ])
  where
    -- label included for error reporting
    knownWidth :: ( Located Label, [ Located BitDataField ]) ->
                  [ Maybe (Type, BDD.Pat) ] ->
                  Maybe (Located Label, BDD.Width)
    knownWidth (ctor, _field) sz_tys = do
      all_sz_tys <- sequenceA sz_tys
      pure (ctor, sum (map (BDD.width . snd) all_sz_tys))

    checkUnderspec (fld, ctors) sz_tys = do
      let nothings = [ loc | (loc, Nothing) <- zip ctors sz_tys ]
      when (length nothings > 1) $
        reportDetailedError fld "Multiple untyped fields, see: "
                                            [ pp (range x) | x <- nothings ]

