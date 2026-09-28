module Daedalus.RTS.HasInputs where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.ByteString(ByteString)
import Data.ByteString.Short(ShortByteString)


class HasInputs a where
  getInputs :: a -> Map ShortByteString ByteString

instance HasInputs () where
  getInputs = const Map.empty
  {-# INLINE getInputs #-}

instance HasInputs a => HasInputs [a] where
  getInputs = Map.unions . map getInputs
  {-# INLINE getInputs #-}

instance HasInputs a => HasInputs (Maybe a) where
  getInputs = maybe Map.empty getInputs
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b) => HasInputs (a,b) where
  getInputs (a,b) = Map.unions [getInputs a, getInputs b]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c) => HasInputs (a,b,c) where
  getInputs (a,b,c) = Map.unions [getInputs a, getInputs b, getInputs c]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d) =>
         HasInputs (a,b,c,d) where
  getInputs (a,b,c,d) =
    Map.unions [getInputs a, getInputs b, getInputs c, getInputs d]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e) =>
         HasInputs (a,b,c,d,e) where
  getInputs (a,b,c,d,e) =
    Map.unions [getInputs a, getInputs b, getInputs c, getInputs d, getInputs e]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f) =>
         HasInputs (a,b,c,d,e,f) where
  getInputs (a,b,c,d,e,f) =
    Map.unions [ getInputs a, getInputs b, getInputs c
               , getInputs d, getInputs e, getInputs f
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g) =>
         HasInputs (a,b,c,d,e,f,g) where
  getInputs (a,b,c,d,e,f,g) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h) =>
         HasInputs (a,b,c,d,e,f,g,h) where
  getInputs (a,b,c,d,e,f,g,h) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i) =>
         HasInputs (a,b,c,d,e,f,g,h,i) where
  getInputs (a,b,c,d,e,f,g,h,i) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i, HasInputs j) =>
         HasInputs (a,b,c,d,e,f,g,h,i,j) where
  getInputs (a,b,c,d,e,f,g,h,i,j) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i, getInputs j
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i, HasInputs j,
          HasInputs k) =>
         HasInputs (a,b,c,d,e,f,g,h,i,j,k) where
  getInputs (a,b,c,d,e,f,g,h,i,j,k) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i, getInputs j, getInputs k
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i, HasInputs j,
          HasInputs k, HasInputs l) =>
         HasInputs (a,b,c,d,e,f,g,h,i,j,k,l) where
  getInputs (a,b,c,d,e,f,g,h,i,j,k,l) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i, getInputs j, getInputs k, getInputs l
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i, HasInputs j,
          HasInputs k, HasInputs l, HasInputs m) =>
         HasInputs (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  getInputs (a,b,c,d,e,f,g,h,i,j,k,l,m) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i, getInputs j, getInputs k, getInputs l
               , getInputs m
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i, HasInputs j,
          HasInputs k, HasInputs l, HasInputs m, HasInputs n) =>
         HasInputs (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  getInputs (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i, getInputs j, getInputs k, getInputs l
               , getInputs m, getInputs n
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i, HasInputs j,
          HasInputs k, HasInputs l, HasInputs m, HasInputs n, HasInputs o) =>
         HasInputs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  getInputs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i, getInputs j, getInputs k, getInputs l
               , getInputs m, getInputs n, getInputs o
               ]
  {-# INLINE getInputs #-}

instance (HasInputs a, HasInputs b, HasInputs c, HasInputs d, HasInputs e,
          HasInputs f, HasInputs g, HasInputs h, HasInputs i, HasInputs j,
          HasInputs k, HasInputs l, HasInputs m, HasInputs n, HasInputs o,
          HasInputs p) =>
         HasInputs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
  getInputs (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) =
    Map.unions [ getInputs a, getInputs b, getInputs c, getInputs d
               , getInputs e, getInputs f, getInputs g, getInputs h
               , getInputs i, getInputs j, getInputs k, getInputs l
               , getInputs m, getInputs n, getInputs o, getInputs p
               ]
  {-# INLINE getInputs #-}
