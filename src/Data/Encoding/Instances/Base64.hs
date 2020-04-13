{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Encoding.Instances.Base64 where

import           Data.Encoding.Internal.Types
import           Data.Encoding.Internal.Class
import           Data.Proxy
import           GHC.TypeLits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.Lazy as BL64

import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Base64.URL.Lazy as BL64URL

toTxt :: Enc ("B64" ': xs) B.ByteString -> Enc ("B64" ': xs) T.Text
toTxt = fmap TE.decodeUtf8

toBts :: Enc ("B64" ': xs) T.Text ->  Enc ("B64" ': xs) B.ByteString
toBts = fmap TE.encodeUtf8

toTxtL :: Enc ("B64" ': xs) BL.ByteString -> Enc ("B64" ': xs) TL.Text
toTxtL = fmap TEL.decodeUtf8

toBtsL :: Enc ("B64" ': xs) TL.Text ->  Enc ("B64" ': xs) BL.ByteString
toBtsL = fmap TEL.encodeUtf8

instance Encode (Enc xs B.ByteString) (Enc ("B64" ': xs) B.ByteString) where
    encode = unPriv . pure . B64.encode . getPayload
instance Decode  (Enc ("B64" ': xs) B.ByteString) (Enc xs B.ByteString) where
    decode = fmap (unPriv . pure) . B64.decode . getPayload
instance DecodeLenient (Enc ("B64" ': xs) B.ByteString) (Enc xs B.ByteString) where
    decodeLenient = unPriv . pure . B64.decodeLenient . getPayload

instance Encode (Enc xs BL.ByteString) (Enc ("B64" ': xs) BL.ByteString) where
    encode = unPriv . pure . BL64.encode . getPayload
instance Decode  (Enc ("B64" ': xs) BL.ByteString) (Enc xs BL.ByteString) where
    decode = fmap (unPriv . pure) . BL64.decode . getPayload
instance DecodeLenient (Enc ("B64" ': xs) BL.ByteString) (Enc xs BL.ByteString) where
    decodeLenient = unPriv . pure . BL64.decodeLenient . getPayload

instance Encode (Enc xs B.ByteString) (Enc ("B64URL" ': xs) B.ByteString) where
    encode = unPriv . pure . B64URL.encode . getPayload
instance Decode  (Enc ("B64URL" ': xs) B.ByteString) (Enc xs B.ByteString) where
    decode = fmap (unPriv . pure) . B64URL.decode . getPayload
instance DecodeLenient (Enc ("B64URL" ': xs) B.ByteString) (Enc xs B.ByteString) where
    decodeLenient = unPriv . pure . B64URL.decodeLenient . getPayload

instance Encode (Enc xs BL.ByteString) (Enc ("B64URL" ': xs) BL.ByteString) where
    encode = unPriv . pure . BL64URL.encode . getPayload
instance Decode  (Enc ("B64URL" ': xs) BL.ByteString) (Enc xs BL.ByteString) where
    decode = fmap (unPriv . pure) . BL64URL.decode . getPayload
instance DecodeLenient (Enc ("B64URL" ': xs) BL.ByteString) (Enc xs BL.ByteString) where
    decodeLenient = unPriv . pure . BL64URL.decodeLenient . getPayload
         