{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Data.Encoding (
    module Data.Encoding
    , module Data.Encoding.Internal.Class
    , Enc
    , RecreateEx(..)
    , UnexpectedDecodeEx(..)
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


