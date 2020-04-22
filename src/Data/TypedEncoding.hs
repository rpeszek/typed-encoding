{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Main Module in typed-encoding. 
--
-- This library allows to specify and work with types like
--
-- @
-- -- Base 64 encoded bytes
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
-- To use this library import this module and one or more "instance" modules.
--
-- Here is list of instance modules available in typed-encoding library itself
--
-- * "Data.TypedEncoding.Instances.Base64"
-- * "Data.TypedEncoding.Instances.ASCII" 
-- * "Data.TypedEncoding.Instances.UTF8" 
-- * "Data.TypedEncoding.Instances.Encode.Sample" 
-- 
-- This list is not intended to be exhaustive, rather separate libraries
-- can provide instances for other encodings and transformations.
--
-- To implement a new encoding import this module and
--
-- * "Data.TypedEncoding.Instances.Support"
--
-- Examples of how to use this library are included in
--
-- * "Examples.TypedEncoding"    
module Data.TypedEncoding (
    module Data.TypedEncoding
    -- * Classes
    , module Data.TypedEncoding.Internal.Class
    -- * Types
    , Enc
    , RecreateEx(..)
    , UnexpectedDecodeEx(..)
    -- * Combinators
    , getPayload 
    , unsafeSetPayload
    , fromEncoding
    , toEncoding
    , showEnc
 ) where

import           Data.TypedEncoding.Internal.Types (Enc
                                              , RecreateEx(..)
                                              , UnexpectedDecodeEx(..)
                                              , getPayload
                                              , unsafeSetPayload
                                              , toEncoding
                                              , fromEncoding
                                              , showEnc
                                              )
import           Data.TypedEncoding.Internal.Class
