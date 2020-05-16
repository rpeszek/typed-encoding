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
{-# LANGUAGE TypeApplications #-}

-- | 'UTF-8' encoding
module Data.TypedEncoding.Instances.Restriction.UTF8 where

import           Data.TypedEncoding.Instances.Support

import           Data.Proxy
-- import           GHC.TypeLits

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE 
import qualified Data.Text.Lazy.Encoding as TEL 

-- import qualified Data.ByteString.Char8 as B8
-- import qualified Data.ByteString.Lazy.Char8 as BL8

import           Data.Either

-- $setup
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XPartialTypeSignatures -XFlexibleInstances -XTypeApplications
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> import Data.TypedEncoding.Internal.Util (proxiedId)
-- >>> let emptyUTF8B = unsafeSetPayload () "" ::  Enc '["r-UTF8"] () B.ByteString 
-- >>> :{  
-- instance Arbitrary (Enc '["r-UTF8"] () B.ByteString) where 
--      arbitrary =  fmap (fromRight emptyUTF8B) 
--                   . flip suchThat isRight 
--                   . fmap (encodeFAll @(Either EncodeEx) @'["r-UTF8"] @(). toEncoding ()) $ arbitrary 
-- :}



-----------------
-- Encodings  --
-----------------

prxyUtf8 = Proxy :: Proxy "r-UTF8"

-- TODO these are quick and dirty

-- | UTF8 encodings are defined for ByteString only as that would not make much sense for Text
--
-- >>> encodeFAll . toEncoding () $ "\xc3\xb1" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Right (MkEnc Proxy () "\195\177")
--
-- >>> encodeFAll . toEncoding () $ "\xc3\x28" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Left (EncodeEx "r-UTF8" (Cannot decode byte '\xc3': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream))
--
-- Following test uses 'verEncoding' helper that checks that bytes are encoded as Right iff they are valid UTF8 bytes
--
-- prop> \(b :: B.ByteString) -> verEncoding b (fmap (fromEncoding . decodeAll . proxiedId (Proxy :: Proxy (Enc '["r-UTF8"] _ _))) . (encodeFAll :: _ -> Either EncodeEx _). toEncoding () $ b)

instance Encodings (Either EncodeEx) xs grps c B.ByteString => Encodings (Either EncodeEx) ("r-UTF8" ': xs) ("r-UTF8" ': grps) c B.ByteString where
    encodings = encodeFEncoder @(Either EncodeEx) @"r-UTF8" @"r-UTF8"


instance EncodeF (Either EncodeEx) (Enc xs c B.ByteString) (Enc ("r-UTF8" ': xs) c B.ByteString) where
    encodeF = implEncodeF_ prxyUtf8 (fmap TE.encodeUtf8 . TE.decodeUtf8')
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c B.ByteString) (Enc ("r-UTF8" ': xs) c B.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr @"r-UTF8" . fmap TE.encodeUtf8 . TE.decodeUtf8')
instance Applicative f => DecodeF f (Enc ("r-UTF8" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implTranP id 


instance Encodings (Either EncodeEx) xs grps c BL.ByteString => Encodings (Either EncodeEx) ("r-UTF8" ': xs) ("r-UTF8" ': grps) c BL.ByteString where
    encodings = encodeFEncoder @(Either EncodeEx) @"r-UTF8" @"r-UTF8"

instance EncodeF (Either EncodeEx) (Enc xs c BL.ByteString) (Enc ("r-UTF8" ': xs) c BL.ByteString) where
    encodeF = implEncodeF_ prxyUtf8 (fmap TEL.encodeUtf8 . TEL.decodeUtf8')
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c BL.ByteString) (Enc ("r-UTF8" ': xs) c BL.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr @"r-UTF8" . fmap TEL.encodeUtf8 . TEL.decodeUtf8')
instance Applicative f => DecodeF f (Enc ("r-UTF8" ': xs) c BL.ByteString) (Enc xs c BL.ByteString) where
    decodeF = implTranP id 

--- Utilities ---

-- | helper function checks that given ByteString, 
-- if is encoded as Left is must be not Utf8 decodable
-- is is encoded as Right is must be Utf8 encodable 
verEncoding :: B.ByteString -> Either err B.ByteString -> Bool
verEncoding bs (Left _) = isLeft . TE.decodeUtf8' $ bs
verEncoding bs (Right _) = isRight . TE.decodeUtf8' $ bs
