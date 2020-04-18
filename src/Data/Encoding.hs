{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Data.Encoding (
    module Data.Encoding
    , module Data.Encoding.Internal.Class
    , Enc
    , unsafeGetPayload 
    -- , unsafeMap
    -- , unsafeApp
    -- , unsafeBind
    , fromEncoding
    , toEncoding
 ) where

import           Data.Encoding.Internal.Types (Enc
                                              , unsafeGetPayload
                                              , toEncoding
                                              , fromEncoding
                                            --   , unsafeMap
                                            --   , unsafeApp
                                            --   , unsafeBind
                                              )
import           Data.Encoding.Internal.Class
import           Data.Encoding.Instances.Base64
import           Data.Encoding.Instances.Encode.Sample

