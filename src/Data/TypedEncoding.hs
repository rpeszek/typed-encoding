
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
-- |
-- = Overview
--
-- This library uses 'GHC.TypeLits' symbols to specify and work with types like
--
-- @
-- -- Base 64 encoded bytes (could represent binary files)
-- Enc '["enc-B64"] () ByteString
--
-- -- Base 64 encoded UTF8 bytes
-- Enc '["enc-B64", "r-UTF8"] () ByteString
--
-- -- Text that contains only ASCII characters
-- Enc '["r-ASCII"] () Text
-- @
--
-- or to do transformations to strings like
--
-- @
-- upper :: Text -> Enc '["do-UPPER"] c Text
-- upper = ...
-- @
--
-- or to define precise types to use with 'toEncString' and 'fromEncString'
-- 
-- @
-- date :: Enc '["r-date-%d/%b/%Y:%X %Z"] () Text
-- date = toEncString ...
-- @
--
-- Primary focus of /type-encodings/ is to provide type safe
--
-- * /encoding/
-- * /decoding/
-- * /validation (recreation)/ (verification of existing payload)
-- * type conversions between encoded types
-- * combinators for creating new encodings from existing encodings (e.g. by applying Boolean rules)
--
-- of string-like data (@ByteString@, @Text@) that is subject of some
-- encoding or formatting restrictions.
--
-- as well as
--
-- * /toEncString/ 
-- * /fromEncString/ 
--
-- conversions.
--
-- = Groups of annotations
--
-- typed-encoding uses type annotations grouped into semantic categories
--
-- == "r-" restriction / predicate
--
-- * /encoding/ is a partial identity
-- * /validation/ is a partial identity (matching encoding)
-- * /decoding/ is identity
--
-- Examples: @"r-UTF8"@, @"r-ASCII"@, upper alpha-numeric bound /r-ban/ restrictions like @"r-ban:999-999-9999"@
--
-- == "do-" transformations 
--
--(not provided in this library other than as /Examples/ "Examples.TypedEncoding.Instances.Do.Sample")
--
-- * /encoding/ applies transformation to the string (could be partial)
-- * /decoding/ - typically none
-- * /validation/ - typically none but, if present, verifies the payload has expected data (e.g. only uppercase chars for "do-UPPER")
--
-- Examples: @"do-UPPER"@, @"do-lower"@, @"do-reverse"@
--
-- == "enc-" data encoding that is not "r-"
--
-- * /encoding/ applies encoding transformation to the string (could be partial)
-- * /decoding/ reverses the transformation (can be now be used as pure function)
-- * /validation/ verifies that the payload has correctly encoded data
--
-- Examples: @"enc-B64"@
-- 
--
-- = Call Site Usage
--
-- To use this library import this module and one or more /instance/ or /combinator/ module.
--
-- Here is list of instance modules available in typed-encoding library itself
--
-- * "Data.TypedEncoding.Instances.Enc.Base64"
-- * "Data.TypedEncoding.Instances.Restriction.Misc" (replaces @Common@ from v0.2)
-- * "Data.TypedEncoding.Instances.Restriction.ASCII" 
-- * "Data.TypedEncoding.Instances.Restriction.UTF8" 
-- * "Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums" (moved from @Combinators@ to @Instances@ in v0.3)
-- 
-- ... and needed conversions. 
--
-- Conversion combinator module structure is similar to the one found in /text/ and /bytestring/ packages.
--
-- Conversion is /typed-encoding/ are safe and reversible!
--
-- Please see comments in 
--    
-- * "Data.TypedEncoding.Conv" 
--
-- for more information.
--
-- The instance list is not intended to be exhaustive, rather separate libraries
-- can provide instances for other encodings and transformations.
--
-- = New encoding instance creation
--
-- To implement a new encoding import
--
-- * "Data.TypedEncoding.Instances.Support"
--
-- = Examples
--
-- Examples of how to use this library are included in
--
-- * "Examples.TypedEncoding"    

module Data.TypedEncoding (
  
    -- * @Enc@ and basic combinators
    Enc
    , toEncoding
    , fromEncoding
    , getPayload
  
    -- * Untyped versions of @Enc@
    , module  Data.TypedEncoding.Common.Types.CheckedEnc

    -- * @Encoding@ and basic combinators
    , Encoding (..)
    , _mkEncoding
    , _mkEncoding1 
    , runEncoding'
    , _runEncoding 
    , runEncoding1'
  
    -- * List of encodings
    , Encodings (..)
    , runEncodings'
    , _runEncodings

    -- * Similar to @Encoding@ and @Encodings@ but cover /Decoding/ and /Validation/
    , module Data.TypedEncoding.Common.Types.Decoding
    , module Data.TypedEncoding.Common.Types.Validation

    -- * @UncheckedEnc@ is an /untyped/ version of Enc that represents not validated encoding      
    , module Data.TypedEncoding.Common.Types.UncheckedEnc

    -- * Laws / properties

    , propSafeDecoding' 
    , _propSafeDecoding
    , propSafeValidatedDecoding'
    , _propSafeValidatedDecoding

    -- * Encoding Classes
    , Encode (..)
    , EncodeAll (..)

    -- * Decoding, Validation and Other Classes
    , module Data.TypedEncoding.Common.Class
  
      -- * Combinators
    , module Data.TypedEncoding.Combinators.Common
    , module Data.TypedEncoding.Combinators.Encode
    , module Data.TypedEncoding.Combinators.Decode
    , module Data.TypedEncoding.Combinators.Validate
    , module Data.TypedEncoding.Combinators.Unsafe
    , module Data.TypedEncoding.Combinators.ToEncStr
    , module Data.TypedEncoding.Combinators.Promotion

    -- * Exceptions 
    , module Data.TypedEncoding.Common.Types.Exceptions

    -- * Other
    , module Data.TypedEncoding.Common.Types.Common

 ) where

import           Data.TypedEncoding.Common.Types.Enc
import           Data.TypedEncoding.Common.Types.Decoding
import           Data.TypedEncoding.Common.Types.Validation

import           Data.TypedEncoding.Common.Types.Common
import           Data.TypedEncoding.Common.Types.CheckedEnc
import           Data.TypedEncoding.Common.Types.UncheckedEnc
import           Data.TypedEncoding.Common.Types.Exceptions
import           Data.TypedEncoding.Common.Class
import           Data.TypedEncoding.Combinators.Common
import           Data.TypedEncoding.Combinators.Encode
import           Data.TypedEncoding.Combinators.Decode
import           Data.TypedEncoding.Combinators.Validate
import           Data.TypedEncoding.Combinators.Unsafe
import           Data.TypedEncoding.Combinators.ToEncStr
import           Data.TypedEncoding.Combinators.Promotion


-- | 
-- Main property that encodings are expected to enforce.
--
-- Decoding is safe and can use @Identity@ instance of 'UnexpectedDecodeErr'.
--
-- Errors are handled during the encoding phase.
propSafeDecoding' :: forall alg nm c str. (Eq c, Eq str) =>
                     Encoding (Either EncodeEx) nm alg c str 
                     -> Decoding (Either UnexpectedDecodeEx) nm alg c str 
                     -> c 
                     -> str 
                     -> Bool
propSafeDecoding' encf decf c str = Right enc0 == edec
   where 
       enc0 = toEncoding c str
       eenc = runEncoding' @alg encf enc0
       edec = case eenc of 
           Right enc -> either (Left . show) Right $ runDecoding' @alg decf enc
           Left enc -> Right enc0 -- quick and dirty, magically decode all encoded failures  


_propSafeDecoding :: forall nm c str alg . (Algorithm nm alg, Eq c, Eq str) => 
                     Encoding (Either EncodeEx) nm alg c str 
                     -> Decoding (Either UnexpectedDecodeEx) nm alg c str 
                     -> c 
                     -> str 
                     -> Bool
_propSafeDecoding = propSafeDecoding' @(AlgNm nm)

-- |
-- Similar to 'propSafeDecoding'' but 'Validation' based.
-- 'Validation' acts as 'Decoding' recovering original payload value.
-- Recovering with validation keeps the encoded value and that value
-- is supposed to decode without error. 
--
-- Expects input of encoded values
propSafeValidatedDecoding' :: forall alg nm c str. (Eq c, Eq str) =>
                     Validation (Either RecreateEx) nm alg c str 
                     -> Decoding (Either UnexpectedDecodeEx) nm alg c str 
                     -> c 
                     -> str 
                     -> Bool
propSafeValidatedDecoding' validf decf c str = edec == echeck
   where 
       enc0 = toEncoding c str
       valfs = validf `ConsV` ZeroV
       eenc = recreateWithValidations @'[alg] @'[nm] valfs enc0 
       (edec :: Either String str, echeck :: Either String str) = case eenc of 
           Right enc -> (either (Left . show) (Right . getPayload) $ runDecoding' @alg decf enc
                         , either (Left . show) (Right . getPayload) $ runValidation' @alg validf enc)
           Left _ -> (Left "not-recovered", Left "not-recovered")  

_propSafeValidatedDecoding :: forall nm c str alg. (Algorithm nm alg, Eq c, Eq str) =>
                     Validation (Either RecreateEx) nm alg c str 
                     -> Decoding (Either UnexpectedDecodeEx) nm alg c str 
                     -> c 
                     -> str 
                     -> Bool
_propSafeValidatedDecoding = propSafeValidatedDecoding' @(AlgNm nm)