
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
-- like r-UFT8 but not for other encodings.
--
-- 'inject' is identity on payloads
--
-- @Superset bigger smaller@ reads as @bigger@ is a superset of @smaller@
class Superset (y :: Symbol) (x :: Symbol) where
    inject :: Enc (x ': xs) c str ->  Enc (y ': xs) c str
    inject = withUnsafeCoerce id


instance Superset x x where

-- prop_Superset :: forall y x xs c str . (Superset y x, Eq str) => Enc (x ': xs) c str -> Bool
-- prop_Superset x = getPayload x == (getPayload . inject @y @x $ x)


class FlattenAs (y :: Symbol) (x :: Symbol) where
    flattenAs ::  Enc (x ': xs) c str ->  Enc '[y] c str
    flattenAs = withUnsafeCoerce id
