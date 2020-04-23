{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- | 
module Data.TypedEncoding.Instances.UTF8 where

import           Data.TypedEncoding.Instances.Support
-- import           Data.TypedEncoding.Internal.Utils (proxiedId)
import           Data.TypedEncoding.Unsafe (withUnsafe)

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

import           GHC.TypeLits
import           Data.Char
import           Control.Arrow
import           Data.Text.Encoding.Error (UnicodeException)

import           Data.Either

-- $setup
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> import Data.TypedEncoding.Internal.Utils (proxiedId)
-- >>> let prxyArb = Proxy :: Proxy (Either EncodeEx (Enc '["r-UTF8"] () B.ByteString))
-- >>> :{
-- >>> instance Arbitrary (Enc '["r-UTF8"] () B.ByteString) where 
--      arbitrary =  fmap (fromRight (emptyUTF8B ())) 
--                   . flip suchThat isRight 
--                   . fmap (proxiedId prxyArb . encodeFAll . toEncoding ()) $ arbitrary 
-- :}

-- | empty string is valid utf8
emptyUTF8B :: c -> Enc '["r-UTF8"] c B.ByteString
emptyUTF8B c = unsafeSetPayload c ""   

-----------------
-- Conversions --
-----------------

-- Right tst = encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- tstTxt = byteString2TextS tst

-- | Type-safer version of @Data.Text.Encoding.encodeUtf8@
--
-- >>> text2ByteStringS $ toEncoding () ("text" :: T.Text)
-- MkEnc Proxy () "text"
text2ByteStringS :: Enc ys c T.Text -> Enc ("r-UTF8" ': ys) c B.ByteString
text2ByteStringS = withUnsafeCoerce TE.encodeUtf8

-- | Type-safer version of Data.Text.Encoding.decodeUtf8
--
-- >>> let Right tst = encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- >>> byteString2TextS tst
-- MkEnc Proxy () "Hello World"
byteString2TextS :: Enc ("r-UTF8" ': ys) c B.ByteString -> Enc ys c T.Text 
byteString2TextS = withUnsafeCoerce TE.decodeUtf8

-- | Identity property "byteString2TextS . text2ByteStringS == id"
-- prop> \t -> t == (fromEncoding . txtBsSIdProp (Proxy :: Proxy '[]) . toEncoding () $ t)
txtBsSIdProp :: Proxy (ys :: [Symbol]) -> Enc ys c T.Text -> Enc ys c T.Text
txtBsSIdProp _ = byteString2TextS . text2ByteStringS 

-- | Identity property "text2ByteStringS . byteString2TextS == id".
--
-- prop> \(t :: Enc '["r-UTF8"] () B.ByteString) -> t == (bsTxtIdProp (Proxy :: Proxy '[]) $ t)
bsTxtIdProp :: Proxy (ys :: [Symbol]) -> Enc ("r-UTF8" ': ys) c B.ByteString -> Enc ("r-UTF8" ': ys) c B.ByteString
bsTxtIdProp _ = text2ByteStringS . byteString2TextS

text2ByteStringL :: Enc ys c TL.Text -> Enc ("r-UTF8" ': ys) c BL.ByteString
text2ByteStringL = withUnsafeCoerce TEL.encodeUtf8

byteString2TextL :: Enc ("r-UTF8" ': ys) c BL.ByteString -> Enc ys c TL.Text 
byteString2TextL = withUnsafeCoerce TEL.decodeUtf8

-----------------
-- Encondings  --
-----------------

prxyUtf8 = Proxy :: Proxy "r-UTF8"

-- TODO these are quick and dirty

-- | UTF8 encodings are defined for ByteString only as that would not make much sense for Text
--
-- >>> encodeFAll . toEncoding () $ "\xc3\xb1" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Right (MkEnc Proxy () "\195\177")
-- >>> encodeFAll . toEncoding () $ "\xc3\x28" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Left (EncodeEx "r-UTF8" (Cannot decode byte '\xc3': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream))
--
-- Following test uses 'verEncoding' helper that checks that bytes are encoded as Right iff they are valid UTF8 bytes
--
-- prop> \(b :: B.ByteString) -> verEncoding b (fmap (fromEncoding . decodeAll . proxiedId (Proxy :: Proxy (Enc '["r-UTF8"] _ _))) . (encodeFAll :: _ -> Either EncodeEx _). toEncoding () $ b)

instance EncodeF (Either EncodeEx) (Enc xs c B.ByteString) (Enc ("r-UTF8" ': xs) c B.ByteString) where
    encodeF = implEncodeF prxyUtf8 (fmap TE.encodeUtf8 . TE.decodeUtf8')
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c B.ByteString) (Enc ("r-UTF8" ': xs) c B.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr prxyUtf8 . fmap TE.encodeUtf8 . TE.decodeUtf8')
instance Applicative f => DecodeF f (Enc ("r-UTF8" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranP id 

instance EncodeF (Either EncodeEx) (Enc xs c BL.ByteString) (Enc ("r-UTF8" ': xs) c BL.ByteString) where
    encodeF = implEncodeF prxyUtf8 (fmap TEL.encodeUtf8 . TEL.decodeUtf8')
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c BL.ByteString) (Enc ("r-UTF8" ': xs) c BL.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr prxyUtf8 . fmap TEL.encodeUtf8 . TEL.decodeUtf8')
instance Applicative f => DecodeF f (Enc ("r-UTF8" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranP id 

--- Utilities ---

-- | helper function checks that given ByteString, 
-- if is encoded as Left is must be not Utf8 decodable
-- is is encoded as Right is must be Utf8 encodable 
verEncoding :: B.ByteString -> Either err B.ByteString -> Bool
verEncoding bs (Left _) = isLeft . TE.decodeUtf8' $ bs
verEncoding bs (Right _) = isRight . TE.decodeUtf8' $ bs
