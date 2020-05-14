
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Internal.Class (
    module Data.TypedEncoding.Internal.Class
    , module Data.TypedEncoding.Internal.Class.Util
    , module Data.TypedEncoding.Internal.Class.Encode
    , module Data.TypedEncoding.Internal.Class.Decode
    , module Data.TypedEncoding.Internal.Class.Recreate   
  ) where

import           Data.TypedEncoding.Internal.Class.Util
import           Data.TypedEncoding.Internal.Class.Encode
import           Data.TypedEncoding.Internal.Class.Decode
import           Data.TypedEncoding.Internal.Class.Recreate

import           Data.TypedEncoding.Internal.Types (Enc(..) 
                                                   , withUnsafeCoerce
                                                   , getPayload
                                                   )
import           Data.Functor.Identity
import           GHC.TypeLits


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

-- | Subsets are useful for restriction encodings
-- like r-UFT8 but should not be used for other encodings.
--
-- This would be dangerous, it would, for example, permit converting encoded binary 
-- @"Enc '["enc-"] c ByteString@ to @"Enc '["enc-"] c Text@, decoding which
-- could result in rutime errors.
--
-- The requirement is that that the decoding in the superset
-- can replace the decoding from injected subset.
--
-- @
-- instance Superset "r-ASCII" "enc-B64" where -- DANGEROUS
-- @
--
-- 'inject' is identity on payloads
--
-- @Superset bigger smaller@ reads as @bigger@ is a superset of @smaller@
class Superset (y :: Symbol) (x :: Symbol) where
    inject :: Enc (x ': xs) c str ->  Enc (y ': xs) c str
    inject = withUnsafeCoerce id

type family IsSuperset (y :: Symbol) (x :: Symbol) :: Bool

instance Superset x x where

-- prop_Superset :: forall y x xs c str . (Superset y x, Eq str) => Enc (x ': xs) c str -> Bool
-- prop_Superset x = getPayload x == (getPayload . inject @y @x $ x)


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
