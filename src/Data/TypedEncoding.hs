{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Main Module in typed-econdings. 
--
-- To use this library import this module and one or more "instance" modules.
--
-- Here is list of instance modules available in typed-econdings library itself
--
-- * "Data.TypedEncoding.Instances.Base64"
-- * "Data.TypedEncoding.Instances.ASCII" 
-- * "Data.TypedEncoding.Instances.UTF8" 
-- * "Data.TypedEncoding.Instances.Encode.Sample" 
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


