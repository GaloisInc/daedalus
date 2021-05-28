{-# Language BlockArguments, RecordWildCards #-}
module PdfMonad.Transformer
  ( PdfT
  , runPdfT, PdfResult(..)
  , PdfParser(..)
  , ObjIndex, R(..), ObjLoc(..)
  , Cipher(..)
  , EncContext(..) 
  , doM
  , module RTS.ParserAPI
  ) where

import Data.ByteString(ByteString)
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad(liftM,ap)

import RTS.ParserAPI


data R = R { refObj :: {-# UNPACK #-} !Int
           , refGen :: {-# UNPACK #-} !Int
           } deriving (Eq,Ord,Show)

type ObjIndex = Map R ObjLoc -- Maps opbject ids to their locations.

data ObjLoc = InFileAt !Int   -- ^ At this index
            | InObj !R !Int   -- ^ In this object stream, at this index.
              deriving Show

newtype PdfT m a = P (RO -> RW -> m (a,RW))

data Cipher = V2RC4 | V4AES | V4RC4 

data EncContext = EncContext 
  { key    :: ByteString
  , robj   :: Int
  , rgen   :: Int 
  , ciph   :: Cipher 
  }

data RO = RO
  { roTopInput  :: Input
  , roObjMap    :: ObjIndex
  , roResolving :: Set R        -- ^ we are currently resolving these
  , encContext  :: Maybe EncContext
  }

data RW = RW
  { rwValidated :: Map R ByteString
    -- | References that are validated (or in the process) at the given type
  }

data PdfResult a = ParseOk a
                 | ParseAmbig [a]
                 | ParseErr ParseError

runPdfT :: Functor m => Input -> ObjIndex -> Maybe EncContext -> PdfT m a -> m a
runPdfT inp objMap ec (P m) = fst <$> m ro rw
  where
  ro = RO { roTopInput = inp, roObjMap = objMap, roResolving = Set.empty, encContext = ec }
  rw = RW { rwValidated = Map.empty }
{-# INLINE runPdfT #-}

doM :: Monad m => m a -> PdfT m a
doM m = P \_ s -> liftS s m
{-# INLINE doM #-}


class BasicParser m => PdfParser m where
  resolving       :: R -> m a -> m a
  extendObjIndex  :: ObjIndex -> m a -> m a
  getObjIndex     :: m ObjIndex
  getTopInput     :: m Input
  isValidated     :: R -> ByteString -> m Bool
  startvalidating :: R -> ByteString -> m ()
  getEncContext   :: m (Maybe EncContext)


instance BasicParser m => PdfParser (PdfT m) where
  resolving r (P m) = P \RO {..} s ->
    if r `Set.member` roResolving
      then liftS s $ pError FromSystem "PdfMonad.resolving" "Reference cycle"
      else m RO { roResolving = Set.insert r roResolving, .. } s

  extendObjIndex prevInFileObjMap (P m) = P \RO{..} ->
    m RO { roObjMap = Map.union roObjMap prevInFileObjMap, .. }
    -- NOTE:
    --  - shadowing object indexes happens often.
    --  - this appears to be functioning correctly, it appears unintuitive but note 
    --     - Map.union is left-biased
    --     - extendObjIndex is called in order from the bottom of the file (last update first)
    --     - thus 'roObjMap' is what has been constructed from the bottom of the file.
    --     - so 'prevInFileObjMap' is an objectMap that is "previous in file" with respect to 'rObjMap'
    
  getObjIndex = P \RO { .. } s -> pure (roObjMap,s)

  getTopInput = P \RO { .. } s -> pure (roTopInput,s)

  isValidated r b = P \_ s ->
    case Map.lookup r (rwValidated s) of
      Nothing -> pure (False,s)
      Just b1 | b == b1 -> pure (True,s)
              | otherwise -> pError FromSystem "IsValidate" msg
        where
        msg = "Reference R:" ++ show (refObj r) ++ ":" ++
              show (refGen r) ++ " used at multiple types " ++
              show b ++ " and " ++ show b1

  startvalidating r b = P \_ RW{..} ->
    pure ((), RW { rwValidated = Map.insert r b rwValidated, .. })

  getEncContext = P \ro rw -> pure (encContext ro, rw)

  {-# INLINE resolving  #-}
  {-# INLINE getObjIndex #-}
  {-# INLINE extendObjIndex #-}
  {-# INLINE getTopInput #-}


instance Monad m => Functor (PdfT m) where
  fmap = liftM
  {-# INLINE fmap #-}

instance Monad m => Applicative (PdfT m) where
  pure a = P \_ s -> pure (a,s)
  (<*>)  = ap
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Monad m => Monad (PdfT m) where
  P m >>= f   = P \ro s -> do (a,s1) <- m ro s
                              let P m1 = f a
                              m1 ro s1
  {-# INLINE (>>=) #-}

liftS :: Monad m => s -> m a -> m (a,s)
liftS s m = do a <- m
               pure (a,s)
{-# INLINE liftS #-}

instance BasicParser m => BasicParser (PdfT m) where
  P m ||| P n     = P \ro s -> m ro s ||| n ro s
  P m <|| P n     = P \ro s -> m ro s <|| n ro s
  pFail e         = doM (pFail e)
  pByte r         = doM (pByte r)
  pEnter l (P m)  = P \ro s -> pEnter l (m ro s)
  pStack          = doM pStack
  pPeek           = doM pPeek
  pSetInput i     = doM (pSetInput i)

  pOffset         = doM pOffset
  pEnd r          = doM (pEnd r)
  pMatch1 r v     = doM (pMatch1 r v)

  pErrorMode e (P m) = P \ro s -> pErrorMode e (m ro s)

  {-# INLINE (|||)      #-}
  {-# INLINE (<||)      #-}
  {-# INLINE pFail      #-}
  {-# INLINE pByte      #-}
  {-# INLINE pEnter     #-}
  {-# INLINE pStack     #-}
  {-# INLINE pPeek      #-}
  {-# INLINE pSetInput  #-}
  {-# INLINE pOffset    #-}
  {-# INLINE pEnd       #-}
  {-# INLINE pMatch1    #-}
  {-# INLINE pErrorMode #-}


