{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Data.Encoding (
    module Data.Encoding
    , module Data.Encoding.Internal.Class
    , module Data.Encoding.Instances.Base64
    , module Data.Encoding.Instances.Encode.Sample
    , Enc
    , unsafeGetPayload 
    , fromEncoding
    , toEncoding
 ) where

import           Data.Encoding.Internal.Types (Enc, unsafeGetPayload, toEncoding, fromEncoding)
import           Data.Encoding.Internal.Class
import           Data.Encoding.Instances.Base64
import           Data.Encoding.Instances.Encode.Sample

