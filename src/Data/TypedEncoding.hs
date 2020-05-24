
-- |
-- = Overview
--
-- This library uses 'GHC.TypeLits' symbols to specify and work with types like
--
-- @
-- -- Base 64 encoded bytes (could represent binary files)
-- Enc '["enc-B64"] ByteString
--
-- -- Base 64 encoded UTF8 bytes
-- Enc '["enc-B64", "r-UTF8"] ByteString
--
-- -- Text that contains only ASCII characters
-- Enc '["r-ASCII"] Text
-- @
--
-- or to do transformations to strings like
--
-- @
-- upper :: Text -> Enc '["do-UPPER"] Text
-- upper = ...
-- @
--
-- or to define precise types to use with 'toEncString' and 'fromEncString'
-- 
-- @
-- date :: Enc '["r-date-%d/%b/%Y:%X %Z"] Text
-- date = toEncString ...
-- @
--
-- Primary focus of type-encodings is to provide type safe
--
-- * /encoding/
-- * /decoding/
-- * /validation (recreation)/ (verification of existing payload)
-- * type conversions between encoded types
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
-- == "bool[Op]:" encodings
--
-- Encodings that are defined in terms of other encodings using boolean algebra.
--
-- (early, beta version)
--
-- Examples: 
--
-- @"boolOr:(r-ban:999-999-9999)(r-ban:(999) 999-9999)"@ 
--
-- "@boolNot:(r-ASCII)"
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
-- * "Data.TypedEncoding.Instances.Restriction.Bool" (experimental / early alpha version, moved from @Combinators@ to @Instances@ in v0.3)
-- * "Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums" (moved from @Combinators@ to @Instances@ in v0.3)
-- * "Data.TypedEncoding.Instances.Do.Sample" 
-- 
-- ... and needed conversions. 
--
-- Conversion combinator module structure is similar to one found in /text/ and /bytestring/ packages
-- And can be found (since 0.2.2) in
--
-- * "Data.TypedEncoding.Conv.Text"
-- * "Data.TypedEncoding.Conv.Text.Encoding"
-- * "Data.TypedEncoding.Conv.Text.Lazy"    
-- * "Data.TypedEncoding.Conv.Text.Lazy.Encoding"
-- * "Data.TypedEncoding.Conv.ByteString.Char8"
-- * "Data.TypedEncoding.Conv.ByteString.Lazy.Char8"
--
-- This list is not intended to be exhaustive, rather separate libraries
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

    -- * Existentially quantified and untyped versions of @Enc@
    , module Data.TypedEncoding.Common.Types.SomeEnc
    , module  Data.TypedEncoding.Common.Types.CheckedEnc

    -- * @Encoding@ and basic combinators
    , Encoding (..)
    , _mkEncoding
    , runEncoding'
    , _runEncoding 
  
    -- * List of encodings
    , Encodings (..)
    , runEncodings'
    , _runEncodings

    -- * Similar to @Encoding@ and @Encodings@ but cover /Decoding/ and /Validation/
    , module Data.TypedEncoding.Common.Types.Decoding
    , module Data.TypedEncoding.Common.Types.Validation

    -- * @UncheckedEnc@ is an /untyped/ version of Enc that represents not validated encoding      
    , module Data.TypedEncoding.Common.Types.UncheckedEnc
 
    -- * Classes
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
import           Data.TypedEncoding.Common.Types.SomeEnc
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