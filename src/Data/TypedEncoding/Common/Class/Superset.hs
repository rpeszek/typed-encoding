
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
-- @since 0.2.2.0
type family IsSuperset (y :: Symbol) (x :: Symbol) :: Bool where
    IsSuperset "r-ASCII" "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-UTF8" = 'True
    IsSuperset y x = IsSupersetOpen y (TakeUntil x ":") (ToList x)

-- |
-- @since 0.2.2.0
type family IsSupersetOpen (y :: Symbol) (x :: Symbol) (xs :: [Symbol]) :: Bool

-- |
-- >>> let Right tstAscii = encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- >>> displ (injectInto @ "r-UTF8" tstAscii)
-- "Enc '[r-UTF8] () (Text Hello World)"
--
-- @since 0.2.2.0
injectInto :: forall y x xs c str . (IsSuperset y x ~ 'True) => Enc (x ': xs) c str ->  Enc (y ': xs) c str
injectInto = withUnsafeCoerce id

-- TODO consider expanding to 
-- _injectInto ::forall y x xs c str . (IsSuperset y x ~ 'True) =>  Enc (x ': xs) c str ->  Enc (Replace x y xs) c str

-- |
-- IsSuperset is not intended for @"enc-"@ encodings. This class is.
-- 
-- It allows to specify constraints that say, for example, that /Base 64/ encodes into 
-- a subset of /ASCII/.
--
-- @since 0.3.0.0
class EncodingSuperset (enc :: Symbol) where
    type EncSuperset enc :: Symbol

    implEncInto :: forall xs c str . Enc (enc ': xs) c str -> Enc (EncSuperset enc ': enc ': xs) c str
    implEncInto = withUnsafeCoerce id
    
{-# WARNING implEncInto "Using this method at the call site may not be backward compatible between minor version upgrades, use _encodesInto instead." #-}

_encodesInto :: forall y enc xs c str r . (IsSuperset y r ~ 'True, EncodingSuperset enc, r ~ EncSuperset enc) => Enc (enc ': xs) c str -> Enc (y ': enc ': xs) c str
_encodesInto = injectInto . implEncInto

-- prop_Superset :: forall y x xs c str . (Superset y x, Eq str) => Enc (x ': xs) c str -> Bool
-- prop_Superset x = getPayload x == (getPayload . inject @y @x $ x)

