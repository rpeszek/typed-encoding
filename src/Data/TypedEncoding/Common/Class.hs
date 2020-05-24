
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

import           Data.TypedEncoding.Common.Types (Enc(..) )
import           Data.TypedEncoding.Combinators.Unsafe (withUnsafeCoerce)

import           GHC.TypeLits



-- | 
-- Generalized Java @toString@ or a type safe version of Haskell's 'Show'.
--
-- Encodes @a@ as @Enc '[xs]@ specifying algorithm @alg@ and using effect @f@ 
--
-- @since 0.2.0.0 
class (KnownSymbol nm, KnownSymbol ann) => ToEncString f nm ann a str where
    toEncF :: a -> f (Enc '[nm] () str)

-- | 
-- Reverse of 'ToEncString' decodes encoded string back to @a@
--
-- @since 0.2.0.0 
class (KnownSymbol nm, KnownSymbol ann) => FromEncString f nm ann a str where
    fromEncF :: Enc '[nm] () str -> f a



-- Other classes --

-- | Flatten is more permissive 'IsSuperset'
-- @
-- instance FlattenAs "r-ASCII" "enc-B64" where -- OK
-- @
-- 
-- Now encoded data has form @Enc '["r-ASCII"] c str@ 
-- and there is no danger of it begin incorrectly decoded.
--
-- @since 0.1.0.0
class FlattenAs (y :: Symbol) (x :: Symbol) where
    flattenAs ::  Enc (x ': xs) c str ->  Enc '[y] c str
    flattenAs = withUnsafeCoerce id
