{-# LANGUAGE EmptyDataDecls, TypeOperators, LambdaCase #-}
module Streams where
import qualified Data.Map as M
import           Data.Map(Map)
import           Types
import           Utils
import           Primitives


-- this should be line 10, we 'include' at line 12:

-- | extractStreamData - since we now know all Lengths:
--   - 1st, with the file offset, read into a bytestring
--   - 2nd, if an ObjStm, decodes/validates the stream
--     - NOTE: this processing done just once, not each time
--       that we "index" into the ObjStm.
extractStreamData ::
     Map ObjId (TopLevelDef' Offset :+: a)
  -> TopLevelDef' Offset
  -> P (TopLevelDef' ByteString)
extractStreamData _dom' (TLD_ObjStm _)    = error "unexpeced ObjStm"
extractStreamData _dom' (TLD_Value v)     = return $ TLD_Value v
extractStreamData dom' (TLD_Stream d off) =
  do
  len  <- getKeyOfType "Length" T_Int d  -- indirect OR direct
  len' <- derefValue dom' len            -- now an integer direct
  decodingParameters
       <- extractDecodingParameters d
  bs   <- decodeStream len' decodingParameters off
  return $ TLD_Stream d bs

derefType2Ref ::
     Map ObjId (TopLevelDef :+: Type2Ref)
  -> Type2Ref
  -> P TopLevelDef
derefType2Ref dom' (Type2Ref oi j) =
  do
  tld       <- derefTLD dom' (oi,0)
  ObjStm ss <- getObjStm tld      -- make sure the object is ObjStm
  s         <- case safeIndex ss j of
                 Just s  -> return s
                 Nothing -> error "bad type2 ref: index out of bounds"
  v <- parseString pValue s
       -- note that streams cannot be inside ObjStm
  return $ TLD_Value v

getObjStm :: TopLevelDef' ByteString -> P ObjStm
getObjStm (TLD_ObjStm x) = return x
getObjStm _              = error "expected ObjStm" -- cannot recover from this
