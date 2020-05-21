
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Common.Class.Superset where

import           Data.TypedEncoding.Common.Util.TypeLits

import           Data.TypedEncoding.Common.Types (Enc(..) )
import           Data.TypedEncoding.Combinators.Unsafe (withUnsafeCoerce)
import           GHC.TypeLits
import           Data.Symbol.Ascii


-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications
-- >>> import           Data.TypedEncoding
-- >>> import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
-- >>> import           Data.TypedEncoding.Instances.Restriction.ASCII ()
-- >>> import           Data.Text as T


-- |
-- @since 0.3
-- Replaces previous @Superset@ typeclass.
--
-- Subsets are useful for restriction encodings
-- like r-UFT8 but should not be used for other encodings as
-- this would be dangerous. For example, considering "enc-" encoding as a superset of "r-" encoding would
-- permit converting encoded binary 
-- @"Enc '["enc-"] c ByteString@ to @"Enc '["r-ASCII"] c ByteString@ and then to @"Enc '["r-ASCII"] c Text@, 
-- which could result in runtime errors.
--
-- The requirement is that that the decoding in the superset
-- can replace the decoding from injected subset.
--
-- @IsSuperset bigger smaller@ reads as @bigger@ is a superset of @smaller@
--
type family IsSuperset (y :: Symbol) (x :: Symbol) :: Bool where
    IsSuperset "r-ASCII" "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-UTF8" = 'True
    IsSuperset y x = IsSupersetOpen y (TakeUntil x ":") (ToList x)

type family IsSupersetOpen (y :: Symbol) (x :: Symbol) (xs :: [Symbol]) :: Bool

-- |
-- >>> let Right tstAscii = encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- >>> displ (injectInto @ "r-UTF8" tstAscii)
-- "MkEnc '[r-UTF8] () (Text Hello World)"
injectInto :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': xs) c str ->  Enc (y ': xs) c str
injectInto = withUnsafeCoerce id


-- prop_Superset :: forall y x xs c str . (Superset y x, Eq str) => Enc (x ': xs) c str -> Bool
-- prop_Superset x = getPayload x == (getPayload . inject @y @x $ x)

