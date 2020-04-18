{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Data.Encoding.Instances.Base64 where

import           Data.Encoding.Internal.Types
import           Data.Encoding.Internal.Class
import           Data.Proxy
import           Data.Functor.Identity
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
import           Data.Encoding.Internal.Unsafe (withUnsafe)

-- these are risky, need to rethink conversions
-- "enc-B64" Text <-> ["enc-B64", "utf8"] Text

-- instance Convert ("enc-B64" ': xs) B.ByteString T.Text where
--     convert = withUnsafe (fmap TE.decodeUtf8)

-- instance Convert ("enc-B64" ': xs) BL.ByteString TL.Text where
--     convert = withUnsafe (fmap TEL.decodeUtf8)

instance Convert ("enc-B64" ': xs) T.Text B.ByteString where
    convert = withUnsafe (fmap TE.encodeUtf8)

instance Convert ("enc-B64" ': xs) TL.Text BL.ByteString where
    convert = withUnsafe (fmap TEL.encodeUtf8)

-- tst :: Enc '["enc-B64"] c B.ByteString -> Enc '["enc-B64"] c T.Text 
-- tst = convert


instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    encodeF = implTranP B64.encode     
instance DecodeF (Either String) (Enc ("enc-B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranF B64.decode 
instance DecodeF Identity (Enc ("enc-B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranP B64.decodeLenient 

instance Applicative f => EncodeF f  (Enc xs c BL.ByteString) (Enc ("enc-B64" ': xs) c BL.ByteString) where
    encodeF = implTranP BL64.encode 
instance DecodeF (Either String) (Enc ("enc-B64" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranF BL64.decode 
instance DecodeF Identity (Enc ("enc-B64" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranP BL64.decodeLenient 

instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64URL" ': xs) c B.ByteString) where
    encodeF = implTranP B64URL.encode 
instance DecodeF (Either String) (Enc ("enc-B64URL" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranF B64URL.decode 
instance DecodeF Identity (Enc ("enc-B64URL" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranP B64URL.decodeLenient 

instance Applicative f => EncodeF f (Enc xs c BL.ByteString) (Enc ("enc-B64URL" ': xs) c BL.ByteString) where
    encodeF = implTranP BL64URL.encode 
instance DecodeF (Either String) (Enc ("enc-B64URL" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranF BL64URL.decode 
instance DecodeF Identity (Enc ("enc-B64URL" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranP BL64URL.decodeLenient 

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("enc-B64" ': xs) c T.Text) where
    encodeF = implTranP (TE.decodeUtf8 . B64.encode . TE.encodeUtf8)   
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("enc-B64" ': xs) c TL.Text) where
    encodeF = implTranP (TEL.decodeUtf8 . BL64.encode . TEL.encodeUtf8)   
