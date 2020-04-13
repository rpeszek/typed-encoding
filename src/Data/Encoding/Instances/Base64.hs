{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Encoding.Instances.Base64 (
 ) where

import           Data.Encoding.Internal.Types
import           Data.Encoding.Internal.Class
import           Data.Proxy
import           GHC.TypeLits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE 

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Base64.URL.Lazy as BL64URL

instance (Encode "B64" B.ByteString) where
    encode = unPriv . pure . B64.encode  
instance (Decode "B64" B.ByteString) where
    decode = B64.decode . getPayload
instance (DecodeLenient "B64" B.ByteString) where
    decodeLenient = B64.decodeLenient . getPayload
instance (Encode "B64" BL.ByteString) where
    encode = unPriv . pure . BL64.encode  
instance (Decode "B64" BL.ByteString) where
    decode = BL64.decode . getPayload 
instance (DecodeLenient "B64" BL.ByteString) where
    decodeLenient = BL64.decodeLenient . getPayload      
instance (Encode "B64URL" B.ByteString) where
    encode = unPriv . pure . B64URL.encode  
instance (Decode "B64URL" B.ByteString) where
    decode = B64URL.decode . getPayload
instance (DecodeLenient "B64URL" B.ByteString) where
    decodeLenient = B64URL.decodeLenient . getPayload                
instance (Encode "B64URL" BL.ByteString) where
    encode = unPriv . pure . BL64URL.encode  
instance (Decode "B64URL" BL.ByteString) where
    decode = BL64URL.decode . getPayload 
instance (DecodeLenient "B64URL" BL.ByteString) where
    decodeLenient = BL64URL.decodeLenient . getPayload           