{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

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
-- * /recreation/ (verification of existing payload)
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
-- * /recreation/ is a partial identity (matching encoding)
-- * /decoding/ is identity
--
-- Examples: @"r-UTF8"@, @"r-ASCII"@, upper alpha-numeric bound /r-ban/ restrictions like @"r-999-999-9999"@
--
-- == "do-" transformations
--
-- * /encoding/ applies transformation to the string (could be partial)
-- * /decoding/ - typically none
-- * /recreation/ - typically none but, if present, verifies the payload has expected data (e.g. only uppercase chars for "do-UPPER")
--
-- Examples: @"do-UPPER"@, @"do-lower"@, @"do-reverse"@
--
-- == "enc-" data encoding that is not "r-"
--
-- * /encoding/ applies encoding transformation to the string (could be partial)
-- * /decoding/ reverses the transformation (can be used as pure function)
-- * /recreation/ verifies that the payload has correctly encoded data
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
-- @"boolOr:(r-ban:FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF)(r-ban:FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)"@ 
--
-- "@boolNot:(r-ASCII)"
--
--
-- = Usage
--
-- To use this library import this module and one or more /instance/ or /combinator/ module.
--
-- Here is list of instance modules available in typed-encoding library itself
--
-- * "Data.TypedEncoding.Instances.Enc.Base64"
-- * "Data.TypedEncoding.Instances.Restriction.Common" 
-- * "Data.TypedEncoding.Instances.Restriction.ASCII" 
-- * "Data.TypedEncoding.Instances.Restriction.UTF8" 
-- * "Data.TypedEncoding.Instances.Restriction.Bool" (moved from @Combinators@ to @Instances@ in v0.3)
-- * "Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums" (moved from @Combinators@ to @Instances@ in v0.3)
-- * "Data.TypedEncoding.Instances.Do.Sample" 
-- * "Data.TypedEncoding.Instances.ToEncString.Common" 
-- 
-- This list is not intended to be exhaustive, rather separate libraries
-- can provide instances for other encodings and transformations.
--
-- To implement a new encoding import this module and
--
-- * "Data.TypedEncoding.Instances.Support"
--
-- Defining annotations with combinators is an alternative to using typeclass instances. 
--
-- Combinator modules with be merged with Instances modules in the future.
--
-- Included combinator modules:
--
--
-- Conversion combinator module structure is similar to one found in @text@ and @bytestring@ packages
-- And can be found (since 0.2.2) in
--
-- * "Data.TypedEncoding.Conv.Text"
-- * "Data.TypedEncoding.Conv.Text.Encoding"
-- * "Data.TypedEncoding.Conv.Text.Lazy"    
-- * "Data.TypedEncoding.Conv.Text.Lazy.Encoding"
-- * "Data.TypedEncoding.Conv.ByteString.Char8"
-- * "Data.TypedEncoding.Conv.ByteString.Lazy.Char8"
--
--
-- = Examples
--
-- Examples of how to use this library are included in
--
-- * "Examples.TypedEncoding"    
module Data.TypedEncoding (
    module Data.TypedEncoding
    -- * Classes
    , module Data.TypedEncoding.Internal.Class
    -- * Encoding class and Encoder (replaces EncodeFAll)
    , module Data.TypedEncoding.Internal.Class.Encoder
    -- * Combinators
    , module Data.TypedEncoding.Internal.Combinators
    -- * Types
    -- TODO v0.3 move back to selective imports?
    , module Data.TypedEncoding.Internal.Types.Enc
    , module Data.TypedEncoding.Internal.Types.Decoding
    , CheckedEnc
    , EncodeEx(..)
    , RecreateEx(..)
    , UnexpectedDecodeEx(..)
    , EncAnn 
    -- * Existentially quantified version of @Enc@ and basic combinators
    , module Data.TypedEncoding.Internal.Types.SomeEnc
    -- * Types and combinators for not verfied encoding 
    , module Data.TypedEncoding.Internal.Types.UncheckedEnc
    -- * Basic @Enc@ Combinators
    
    --, getPayload 
    --, unsafeSetPayload
    --, fromEncoding
    --, toEncoding
    -- * Basic @CheckedEnc@ Combinators  
    , unsafeCheckedEnc
    , getCheckedPayload
    , getCheckedEncPayload
    , toCheckedEnc
    , fromCheckedEnc
    -- * Other Basic Combinators     
    , recreateErrUnknown
 ) where

import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Types.Enc
import           Data.TypedEncoding.Internal.Types.Decoding
 
import           Data.TypedEncoding.Internal.Types.SomeEnc
import           Data.TypedEncoding.Internal.Types.UncheckedEnc
import           Data.TypedEncoding.Internal.Class
import           Data.TypedEncoding.Internal.Combinators
import           Data.TypedEncoding.Internal.Class.Encoder
