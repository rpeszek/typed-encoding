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

toTxt :: Enc ("B64" ': xs) c B.ByteString -> Enc ("B64" ': xs) c T.Text
toTxt = fmap TE.decodeUtf8

toBts :: Enc ("B64" ': xs) c T.Text ->  Enc ("B64" ': xs) c B.ByteString
toBts = fmap TE.encodeUtf8

toTxtL :: Enc ("B64" ': xs) c BL.ByteString -> Enc ("B64" ': xs) c TL.Text
toTxtL = fmap TEL.decodeUtf8

toBtsL :: Enc ("B64" ': xs) c TL.Text ->  Enc ("B64" ': xs) c BL.ByteString
toBtsL = fmap TEL.encodeUtf8

instance Encode (Enc xs c B.ByteString) (Enc ("B64" ': xs) c B.ByteString) where
    encode = implTran B64.encode 
instance Decode  (Enc ("B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decode = implTranErr B64.decode 
instance DecodeLenient (Enc ("B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeLenient = implTran B64.decodeLenient 

instance Encode (Enc xs c BL.ByteString) (Enc ("B64" ': xs) c BL.ByteString) where
    encode = implTran BL64.encode 
instance Decode  (Enc ("B64" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decode = implTranErr BL64.decode 
instance DecodeLenient (Enc ("B64" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeLenient = implTran BL64.decodeLenient 

instance Encode (Enc xs c B.ByteString) (Enc ("B64URL" ': xs) c B.ByteString) where
    encode = implTran B64URL.encode 
instance Decode  (Enc ("B64URL" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decode = implTranErr B64URL.decode 
instance DecodeLenient (Enc ("B64URL" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeLenient = implTran B64URL.decodeLenient 

instance Encode (Enc xs c BL.ByteString) (Enc ("B64URL" ': xs) c BL.ByteString) where
    encode = implTran BL64URL.encode 
instance Decode  (Enc ("B64URL" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decode = implTranErr BL64URL.decode 
instance DecodeLenient (Enc ("B64URL" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeLenient = implTran BL64URL.decodeLenient 
         