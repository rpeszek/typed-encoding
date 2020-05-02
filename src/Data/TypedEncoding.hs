{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}

-- |
-- = Overview
--
-- This library allows to specify and work with types like
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
-- or define precise types to use with 'toEncString' and 'fromEncString'
-- 
-- @
-- ipAdd :: Enc '["r-IpV4"] Text
-- ipAdd = toEncString ...
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
-- Examples: @"r-UTF8"@, @"r-ASCII"@
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
-- = Usage
--
-- To use this library import this module and one or more /instance/ module.
--
-- Here is list of instance modules available in typed-encoding library itself
--
-- * "Data.TypedEncoding.Instances.Enc.Base64"
-- * "Data.TypedEncoding.Instances.Restriction.Common" 
-- * "Data.TypedEncoding.Instances.Restriction.ASCII" 
-- * "Data.TypedEncoding.Instances.Restriction.UTF8" 
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
-- = Examples
--
-- Examples of how to use this library are included in
--
-- * "Examples.TypedEncoding"    
module Data.TypedEncoding (
    module Data.TypedEncoding
    -- * Classes
    , module Data.TypedEncoding.Internal.Class
    -- * Combinators
    , module Data.TypedEncoding.Internal.Combinators
    -- * Types
    , Enc
    , SomeEnc
    , Unchecked (..)
    , EncodeEx(..)
    , RecreateEx(..)
    , UnexpectedDecodeEx(..)
    , SomeAnn 
    -- * Basic @Enc@ Combinators
    , getPayload 
    , unsafeSetPayload
    , fromEncoding
    , toEncoding
    -- * Basic @SomeEnc@ Combinators  
    , unsafeSomeEnc
    , getSomePayload
    , getSomeEncPayload
    , toSomeEnc
    , fromSomeEnc
    -- * Basic @Unchecked@ Combinators
    , toUnchecked
    , getUncheckedAnn
    , verifyAnn
     -- * Other Basic Combinators     
    , recreateErrUnknown
 ) where

-- import           Data.TypedEncoding.Internal.Types (Enc
--                                               , RecreateEx(..)
--                                               , UnexpectedDecodeEx(..)
--                                               , EncodeEx(..)
--                                               , getPayload
--                                               , unsafeSetPayload
--                                               , toEncoding
--                                               , fromEncoding
--                                               , SomeEnc
--                                                )
import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Class
import           Data.TypedEncoding.Internal.Combinators
