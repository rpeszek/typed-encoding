
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

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
                                                   , withUnsafeCoerce)
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits
import           Data.Semigroup ((<>))


-- TODO would be nice to have EncodeFAll1 and DecodeFAll1 that starts
-- end stops at frist encoding.           

-- | Generalized Java toString, it encodes @a@ as @Enc@
-- It is here to generate data that can be then partially encoded
-- using encoding combinators.
-- It is not part of encodeFAll, so it instances do not need to use polymorphic @f@ effect.
class KnownSymbol x => ToEncString x str f a where
    toEncStringF :: a -> f (Enc '[x] () str)

toEncString :: forall x str f a  . (ToEncString x str Identity a) => a -> Enc '[x] () str
toEncString = runIdentity . toEncStringF


class (KnownSymbol x) => FromEncString a f str x where
    fromEncStringF :: Enc '[x] () str -> f a

fromEncString :: forall a str x . (FromEncString a Identity str x) => Enc '[x] () str -> a
fromEncString = runIdentity . fromEncStringF

-- Other classes --

-- subsets are useful for restriction encodings
-- like r-UFT8 but not for other encodings.
class Subset (x :: k) (y :: k) where
    inject :: Proxy y -> Enc (x ': xs) c str ->  Enc (y ': xs) c str
    inject _ = withUnsafeCoerce id

class FlattenAs (x :: k) (y :: k) where
    flattenAs :: Proxy y -> Enc (x ': xs) c str ->  Enc '[y] c str
    flattenAs _ = withUnsafeCoerce id






