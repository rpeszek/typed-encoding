{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Data.Encoding (
    module Data.Encoding
    , module Data.Encoding.Internal.Class
    , Enc
    , unsafeGetPayload 
    , fromEncoding
    , toEncoding
    , showEnc
 ) where

import           Data.Encoding.Internal.Types (Enc
                                              , unsafeGetPayload
                                              , toEncoding
                                              , fromEncoding
                                              , showEnc
                                              )
import           Data.Encoding.Internal.Class


