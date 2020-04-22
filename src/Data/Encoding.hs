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
-- * "Data.Encoding.Instances.Base64"
-- * "Data.Encoding.Instances.ASCII" 
-- * "Data.Encoding.Instances.UTF8" 
-- * "Data.Encoding.Instances.Encode.Sample" 
-- 
-- To implement a new encoding import this module and
--
-- * "Data.Encoding.Instances.Support"
--
-- Examples of how to use this library are included in
--
-- * "Examples"    
module Data.Encoding (
    module Data.Encoding
    -- * Classes
    , module Data.Encoding.Internal.Class
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

import           Data.Encoding.Internal.Types (Enc
                                              , RecreateEx(..)
                                              , UnexpectedDecodeEx(..)
                                              , getPayload
                                              , unsafeSetPayload
                                              , toEncoding
                                              , fromEncoding
                                              , showEnc
                                              )
import           Data.Encoding.Internal.Class


