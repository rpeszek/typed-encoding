
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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- |
-- This module contains typeclasses and type families that are used by /typed-encoding/ to 
-- define subset / superset relationships between different encodings. 
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.

module Data.TypedEncoding.Common.Class.Superset where

import           Data.TypedEncoding.Common.Util.TypeLits

import           Data.TypedEncoding.Common.Types (Enc(..)
                                                  , EncodeEx(..)
                                                  , Encoding
                                                  , getPayload 
                                                  , runEncoding'
                                                  , toEncoding
                                                  , AlgNm)
import           Data.TypedEncoding.Combinators.Unsafe (withUnsafeCoerce)
import           GHC.TypeLits
import           Data.Symbol.Ascii
import           Data.Either (isLeft)


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
-- Note, no IsSuperset "r-UNICODE.D76" "r-CHAR8" even though the numeric range of D76 includes all CHAR8 bytes.
-- This is more /nominal/ decision that prevents certain unwanted conversions from being possible.
--
-- @since 0.2.2.0
type family IsSuperset (y :: Symbol) (x :: Symbol) :: Bool where
    IsSuperset "r-ASCII" "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-ASCII" = 'True
    IsSuperset "r-UTF8"  "r-UTF8" = 'True
    IsSuperset "r-CHAR8" "r-ASCII" = 'True  -- "r-CHAR8" is phantom, no explicit instances so it does not need reflexive case
    IsSuperset "r-CHAR8" "r-ByteRep" = 'True
    IsSuperset "r-UNICODE.D76" "r-UNICODE.D76" = 'True 
    IsSuperset "r-UNICODE.D76" "r-ASCII" = 'True 
    IsSuperset "r-UNICODE.D76" x = Or (IsSuperset "r-CHAR8" x) (IsSupersetOpen "r-UNICODE.D76" x (TakeUntil x ":") (ToList x))
    IsSuperset "r-CHAR8" x = Or (IsSuperset "r-ASCII" x) (IsSupersetOpen "r-CHAR8" x (TakeUntil x ":") (ToList x))
    IsSuperset y x = IsSupersetOpen y x (TakeUntil x ":") (ToList x)

-- TODO introduce "r-NODEC" which does not decode


-- |
-- @since 0.2.2.0
type family IsSupersetOpen (big :: Symbol) (nm :: Symbol) (alg :: Symbol) (nmltrs :: [Symbol]) :: Bool

type Superset big small = (IsSuperset big small ~ 'True)

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


propSuperset' :: forall algb algs b s str . (Superset b s, Eq str) 
                 => 
                 Encoding (Either EncodeEx) b algb () str 
                 -> Encoding (Either EncodeEx) s algs () str 
                 -> str 
                 -> Bool
propSuperset' = propSupersetCheck @algb @algs
{-# DEPRECATED propSuperset' "Use propSupersetCheck or propSuperset_" #-}
        
propSuperset_ :: forall b s str algb algs. (Superset b s, Eq str, AlgNm b ~ algb, AlgNm s ~ algs) => Encoding (Either EncodeEx) b algb () str -> Encoding (Either EncodeEx) s algs () str -> str -> Bool
propSuperset_ = propSupersetCheck @algb @algs

-- |
-- Test for Supersets defined in this module
--
-- Actual tests in the project /test/ suite.
propSupersetCheck :: forall algb algs b s str . (Eq str) 
                 => 
                 Encoding (Either EncodeEx) b algb () str 
                 -> Encoding (Either EncodeEx) s algs () str 
                 -> str 
                 -> Bool
propSupersetCheck encb encs str = 
    case (isLeft rb, isLeft rs) of 
        (True, False) -> False
        (False, False) -> pb == ps
        _ -> True

    where 
        rb = runEncoding' @algb encb . toEncoding () $ str 
        rs = runEncoding' @algs encs . toEncoding () $ str 
        pb = either (const str) id $ getPayload <$> rb
        ps = either (const str) id $ getPayload <$> rs

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


propEncodesInto_ :: forall b r str algb algr. (
    EncodingSuperset b
    , r ~ EncSuperset b
    , Eq str
    , AlgNm b ~ algb
    , AlgNm r ~ algr
    ) => Encoding (Either EncodeEx) b algb () str 
       -> Encoding (Either EncodeEx) r algr () str 
       -> str 
       -> Bool
propEncodesInto_ = propEncodesIntoCheck @algb @algr

-- |
-- validates superset restriction
-- 
-- Actual tests are in the project /test/ suite.
propEncodesIntoCheck :: forall algb algr b r str . (Eq str) => Encoding (Either EncodeEx) b algb () str -> Encoding (Either EncodeEx) r algr () str -> str -> Bool
propEncodesIntoCheck encb encr str = 
   case runEncoding' @algb encb . toEncoding () $ str of
            Right r -> case runEncoding' @algr encr . toEncoding () $ getPayload r of
                Left _ -> False
                Right _ -> True
            Left _ -> True  

-- | Checks if first encoding exceptions less often than second (has bigger domain).
propCompEncoding :: forall algb algr b r str .  Encoding (Either EncodeEx) b algb () str -> Encoding (Either EncodeEx) r algr () str -> str -> Bool
propCompEncoding encb encr str = 
    case rr of
        Left _ -> True
        Right _ -> case runEncoding' @algb encb . toEncoding () $ str of
            Right _ -> True
            Left _ -> False  
    where
        rr = runEncoding' @algr encr . toEncoding () $ str 


-- | 
-- Aggregate version of 'EncodingSuperset' 
--
-- It is used to assure type safety of conversion functions in "Data.TypedEncoding.Conv".
-- This approach is not ideal since
-- it produces an overly conservative restriction on encoding stack. 
--
-- The issue is that this enforces restriction on the co-domain or each encoding and it does not take 
-- into account the fact that the domain is already restricted, e.g. it will prevent adding id transformation to the stack.
--
-- @since 0.4.0.0
class AllEncodeInto (superset :: Symbol) (encnms :: [Symbol]) where
     
instance {-# OVERLAPPING #-}  AllEncodeInto "r-CHAR8" '[] 
instance (AllEncodeInto "r-CHAR8" xs, IsSuperset "r-CHAR8" r ~ 'True, EncodingSuperset enc, r ~ EncSuperset enc) => AllEncodeInto "r-CHAR8" (enc ': xs)

instance {-# OVERLAPPING #-}  AllEncodeInto "r-UNICODE.D76" '[] 
instance (AllEncodeInto "r-UNICODE.D76" xs, IsSuperset "r-UNICODE.D76" r ~ 'True, EncodingSuperset enc, r ~ EncSuperset enc) => AllEncodeInto "r-UNICODE.D76" (enc ': xs)

instance {-# OVERLAPPING #-}  AllEncodeInto "r-UTF8" '[] 
instance (AllEncodeInto "r-UTF8" xs, IsSuperset "r-UTF8" r ~ 'True, EncodingSuperset enc, r ~ EncSuperset enc) => AllEncodeInto "r-UTF8" (enc ': xs)

tstChar8Encodable :: forall nms . AllEncodeInto "r-CHAR8" nms => String
tstChar8Encodable = "I am CHAR8 encodable"

tstD76Encodable :: forall nms . AllEncodeInto "r-UNICODE.D76" nms => String
tstD76Encodable = "I can be a valid text"

tstUTF8Encodable :: forall nms . AllEncodeInto "r-UTF8" nms => String
tstUTF8Encodable = "I am a valid UTF8"