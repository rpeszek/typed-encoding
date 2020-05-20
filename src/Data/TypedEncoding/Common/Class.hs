
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

module Data.TypedEncoding.Common.Class (
    module Data.TypedEncoding.Common.Class
    , module Data.TypedEncoding.Common.Class.Util
    , module Data.TypedEncoding.Common.Class.Encode
    , module Data.TypedEncoding.Common.Class.Decode
    , module Data.TypedEncoding.Common.Class.Validate 
    , module Data.TypedEncoding.Common.Class.Superset 
  ) where

import           Data.TypedEncoding.Common.Class.Util
import           Data.TypedEncoding.Common.Class.Encode
import           Data.TypedEncoding.Common.Class.Decode
import           Data.TypedEncoding.Common.Class.Validate
import           Data.TypedEncoding.Common.Class.Superset

import           Data.TypedEncoding.Common.Types (Enc(..) 
                                                   , withUnsafeCoerce
                                                   -- , getPayload
                                                   )
import           Data.Functor.Identity
import           GHC.TypeLits



-- | 
-- Generalized Java @toString@ or a type safe version of Haskell's 'Show'.
--
-- Encodes @a@ as @Enc '[xs]@ specifying algorithm @alg@ and using effect @f@ 
--
class (KnownSymbol nm, KnownSymbol ann) => ToEncString f nm ann a str where
    toEncF :: a -> f (Enc '[nm] () str)


-- backward compatible v0.2 like combinators
toEncStringF :: forall nm f a str  . (ToEncString f nm nm a str) => a -> f (Enc '[nm] () str)
toEncStringF = toEncStringF' @nm @nm

toEncStringF' :: forall alg nm f a str  . (ToEncString f nm alg a str) => a -> f (Enc '[nm] () str)
toEncStringF' = toEncF @f @nm @alg

toEncString :: forall nm a str  . (ToEncString Identity nm nm a str) => a -> Enc '[nm] () str
toEncString = toEncString' @nm @nm

toEncString' :: forall alg nm a str  . (ToEncString Identity nm alg a str) => a -> Enc '[nm] () str
toEncString' = runIdentity . toEncF @Identity @nm @alg


-- | 
-- Reverse of 'ToEncString' decodes encoded string back to @a@
class (KnownSymbol nm, KnownSymbol ann) => FromEncString f nm ann a str where
    fromEncF :: Enc '[nm] () str -> f a

-- backward compatible v0.2 like combinators
fromEncStringF :: forall nm f a str  . (FromEncString f nm nm a str) => Enc '[nm] () str -> f a
fromEncStringF = fromEncStringF' @nm @nm

fromEncStringF' :: forall alg nm f a str  . (FromEncString f nm alg a str) => Enc '[nm] () str -> f a
fromEncStringF' = fromEncF @f @nm @alg

fromEncString :: forall nm a str  . (FromEncString Identity nm nm a str) => Enc '[nm] () str -> a
fromEncString = fromEncString' @nm @nm

fromEncString' :: forall alg nm a str  . (FromEncString Identity nm alg a str) => Enc '[nm] () str -> a
fromEncString' = runIdentity . fromEncF @Identity @nm @alg


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