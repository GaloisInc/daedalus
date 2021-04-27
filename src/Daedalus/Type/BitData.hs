{-# Language ViewPatterns #-}
{-# Language OverloadedStrings #-}

module Daedalus.Type.BitData (inferBitData) where

import Control.Monad (zipWithM_, zipWithM, when, unless)
import Data.Maybe (catMaybes)
import Data.Bits  (shiftL, (.|.) )

import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Panic
import Daedalus.Pass
import Daedalus.SourceRange

import Daedalus.AST
import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Kind

srcTypeToSizedType :: SrcType -> TypeM Grammar (Type, Int)
srcTypeToSizedType sTy = do
  ty <- checkType KValue sTy
  let badType msg = reportError sTy ("Type " <> backticks (pp ty) <> " cannot be used as bitdata" <> msg)
  n <- case ty of
    TCon n [] -> do
      m_decl <- lookupTypeDef n
      case m_decl of
        Nothing -> reportError sTy ("Unknown type " <> pp ty)
        Just decl | Just w <- tctyBDWidth decl -> pure w
        _ -> badType "(type has unknown size)"
    TVar {} -> badType "(unexpected type variable)"
    Type (TUInt (Type (TNum n))) -> pure (fromInteger n)
    Type (TSInt (Type (TNum n))) -> pure (fromInteger n)
    Type (TUInt {}) -> badType "(type has non-constant size)"
    Type (TSInt {}) -> badType "(type has non-constant size)"
    _ -> badType "(type cannot be used in bitdata)"

  pure (ty, n)

-- FIXME: we don't care (?) about Grammar here, it is just here to make TC work    
bdfSizedType :: BitDataField -> TypeM Grammar (Maybe (Type, Int))
bdfSizedType bdf =
  case bdf of
    BDFLiteral _ m_sty -> traverse srcTypeToSizedType m_sty
    BDFField   _ m_sty -> traverse srcTypeToSizedType m_sty
    BDFWildcard  m_sty -> traverse srcTypeToSizedType m_sty

inferCtor :: Name -> Int -> (Located Label, [ Located BitDataField ]) -> [Maybe (Type, Int)] ->
                   TypeM Grammar (TCTyDecl, (Label, (Type, Maybe TCBDUnionMeta)))
inferCtor tyN w (ctor, fields) m_sz_tys = do
  sz_tys <- resolveMissingTypes m_sz_tys
  let (_tys, szs) = unzip sz_tys

  zipWithM_ checkLiteralWidth fields szs
  
  let umeta = mkUnionMeta 0 0 (zip fields szs)
  let def = TCTyStruct (mkFields [] w (zip fields sz_tys))
      
  n' <- TCTy <$> deriveNameWith mkIdent tyN
  let decl = TCTyDecl { tctyName    = n'
                      , tctyParams  = []
                      , tctyBDWidth = Just w
                      , tctyDef     = def
                      }
      ty   = TCon n' []
      
  pure (decl, (thingValue ctor, (ty, Just umeta)))
  
  where
    mkIdent ident = ident <> "_" <> thingValue ctor
    
    -- This assumes the high bits are stored as the first fields
    mkUnionMeta macc bacc [] = TCBDUnionMeta { tcbduMask = macc, tcbduBits = bacc }
    mkUnionMeta macc bacc ((fld, n) : rest) =
      let (mask, v) = case thingValue fld of
            BDFLiteral l _ -> (2 ^ n - 1, l)
            _              -> (0, 0)
      in mkUnionMeta ((macc `shiftL` n) .|. mask) ((bacc `shiftL` n) .|. v) rest

    mkFields acc 0 [] = reverse acc -- FIXME: check w' == 0 (should be?)
    mkFields _acc w' [] = panic "Saw non-zero remainder" [showPP w']
    mkFields acc w' ((fld, (ty, n)) : rest) =
      let acc' = case thingValue fld of
            BDFField l _ -> (l, (ty, Just $ TCBDStructMeta { tcbdsLowBit = w' - n, tcbdsWidth  = n}))
                            : acc
            _            -> acc
      in mkFields acc' (w' - n) rest
    
    resolveMissingTypes widths = do
      -- width of known fields
      
      let knownWidths = sum (map snd (catMaybes widths))
      when (w < knownWidths) $
        reportDetailedError ctor "Cannot instantiate missing type as type is too wide"
                            [ "Type width:" <+> pp w
                            , "Width of known fields:" <+> pp knownWidths
                            ]

      pure (map (resolveField (w - knownWidths)) widths)

    resolveField :: Int -> Maybe (Type, Int) -> (Type, Int)
    resolveField _w (Just x) = x
    resolveField w' Nothing   = (tUInt (tNum (fromIntegral w')), w')

    checkLiteralWidth v@(thingValue -> BDFLiteral l _) n = 
      unless (l < 2 ^ n) $
        reportError v ("Literal value" <+> backticks (pp l) <+> "does not fit into type width of" <+> pp n)
    checkLiteralWidth _ _ = pure ()
                               
inferBitData :: BitData -> MTypeM (Map TCTyName TCTyDecl)
inferBitData bd = runSTypeM . runTypeM (bdName bd) $ do
  m_sz_tyss <- mapM (mapM (bdfSizedType . thingValue) . snd ) (bdCtors bd)
  
  -- Check that no rule has more than 1 underspecified width
  zipWithM_ checkUnderspec (bdCtors bd) m_sz_tyss

  -- Get the first known type width, if it exists (error otherwise)
  let knownWidths = catMaybes (zipWith knownWidth (bdCtors bd) m_sz_tyss)
  width <- case knownWidths of
    []            -> reportError bd "Unable to determine size of type"
    (_fld, w) : _ -> pure w
      
  -- Check all known widths are the same
  unless (all ((==) width . snd) knownWidths) $
    -- FIXME: we know the fields, produce a better error message (after figuring out what that looks like)
    reportError bd "Mismatched field widths" 
  
  (decls, ctors) <- unzip <$> zipWithM (inferCtor (bdName bd) width) (bdCtors bd) m_sz_tyss

  let decl = TCTyDecl { tctyName    = TCTy (bdName bd)
                      , tctyParams  = []
                      , tctyBDWidth = Just width
                      , tctyDef     = TCTyUnion ctors
                      }

  pure (Map.fromList [ (tctyName d, d) | d <- decl : decls ])
  where
    -- label included for error reporting
    knownWidth :: ( Located Label, [ Located BitDataField ]) ->
                  [ Maybe (Type, Int) ] ->
                  Maybe (Located Label, Int)                  
    knownWidth (ctor, _field) sz_tys = do
      all_sz_tys <- sequenceA sz_tys
      pure (ctor, sum (map snd all_sz_tys))
    
    checkUnderspec (fld, ctors) sz_tys = do
      let nothings = [ loc | (loc, Nothing) <- zip ctors sz_tys ]
      when (length nothings > 1) $
        reportDetailedError fld "Multiple untyped fields, see: " [ pp (range x) | x <- nothings ]

