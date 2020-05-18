
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Internal.Class (
    module Data.TypedEncoding.Internal.Class
    , module Data.TypedEncoding.Internal.Class.Util
    , module Data.TypedEncoding.Internal.Class.Encode
    , module Data.TypedEncoding.Internal.Class.Decode
    , module Data.TypedEncoding.Internal.Deprecated.Encode
    , module Data.TypedEncoding.Internal.Deprecated.Decode
    , module Data.TypedEncoding.Internal.Class.Recreate 
    , module Data.TypedEncoding.Internal.Class.Superset 
    -- * Encoder and Encoding replace EncodeFAll
    , module Data.TypedEncoding.Internal.Class.Encoder 
  ) where

import           Data.TypedEncoding.Internal.Class.Util
import           Data.TypedEncoding.Internal.Class.Encode
import           Data.TypedEncoding.Internal.Class.Decode
import           Data.TypedEncoding.Internal.Deprecated.Encode
import           Data.TypedEncoding.Internal.Deprecated.Decode
import           Data.TypedEncoding.Internal.Class.Recreate
import           Data.TypedEncoding.Internal.Class.Superset
import           Data.TypedEncoding.Internal.Class.Encoder 

import           Data.TypedEncoding.Internal.Types (Enc(..) 
                                                   , withUnsafeCoerce
                                                   -- , getPayload
                                                   )
import           Data.Functor.Identity
import           GHC.TypeLits


-- TODO v0.3 change to @ToEncString nm alg str f a@ ? Maybe not?

-- | 
-- Generalized Java @toString@ or a type safe version of Haskell's 'Show'.
--
-- Encodes @a@ as @Enc '[xs]@.
--
class KnownSymbol x => ToEncString x str f a where
    toEncStringF :: a -> f (Enc '[x] () str)

toEncString :: forall x str f a  . (ToEncString x str Identity a) => a -> Enc '[x] () str
toEncString = runIdentity . toEncStringF


-- | 
-- Reverse of 'ToEncString' decodes encoded string back to @a@
class (KnownSymbol x) => FromEncString a f str x where
    fromEncStringF :: Enc '[x] () str -> f a

fromEncString :: forall a str x . (FromEncString a Identity str x) => Enc '[x] () str -> a
fromEncString = runIdentity . fromEncStringF

-- Other classes --

-- | Flatten is more permissive than 'Superset'
-- @
-- instance FlattenAs "r-ASCII" "enc-B64" where -- OK
-- @
-- 
-- Now encoded data has form @Enc '["r-ASCII"] c str@ 
-- and there is no danger of it begin incorrectly decoded.

class FlattenAs (y :: Symbol) (x :: Symbol) where
    flattenAs ::  Enc (x ': xs) c str ->  Enc '[y] c str
    flattenAs = withUnsafeCoerce id
