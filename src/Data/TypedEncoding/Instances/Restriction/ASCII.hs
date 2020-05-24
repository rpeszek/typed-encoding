{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Strings can be encoded as 'Enc "r-ASCII"@ only if they contain only ASCII characters (first 128 characters of the Unicode character set).
--
-- This is sometimes referred to as ASCII-7 and future versions of @type-encoding@ may change @"r-ASCII"@ symbol annotation to reflect this.
--  
-- prop> B8.all ((< 128) . ord) . getPayload @ '["r-ASCII"] @() @B.ByteString
-- 
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds
-- >>> encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- Right (UnsafeMkEnc Proxy () "Hello World")
--
-- >>> encodeFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- Left (EncodeEx "r-ASCII" (NonAsciiChar '\194'))
--
-- @since 0.1.0.0
module Data.TypedEncoding.Instances.Restriction.ASCII where

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Common.Class.Util.StringConstraints

import           Data.TypedEncoding.Internal.Util (explainBool)
import           Data.Char


-- $setup
-- >>> :set -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.ByteString as B
-- >>> import qualified Data.ByteString.Char8 as B8
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> import Data.TypedEncoding
-- >>> :{
-- instance Arbitrary (Enc '["r-ASCII"] () B.ByteString) where 
--      arbitrary =  fmap (unsafeSetPayload ()) 
--                   . flip suchThat (B8.all isAscii) 
--                        $ arbitrary 
-- :}
--


-----------------
-- Encodings  --
-----------------

newtype NonAsciiChar = NonAsciiChar Char deriving (Eq, Show)

-- * Encoding 

instance Encode (Either EncodeEx) "r-ASCII" "r-ASCII" c Char where
    encoding = encASCIIChar    

instance Char8Find str => Encode (Either EncodeEx) "r-ASCII" "r-ASCII" c str where
    encoding = encASCII

encASCIIChar :: Encoding (Either EncodeEx) "r-ASCII" "r-ASCII" c Char 
encASCIIChar = _implEncodingEx (\c -> explainBool NonAsciiChar (c, isAscii c))    

encASCII :: Char8Find str =>  Encoding (Either EncodeEx) "r-ASCII" "r-ASCII" c str
encASCII = _implEncodingEx @"r-ASCII" encImpl

encImpl :: Char8Find str => str -> Either NonAsciiChar str
encImpl str = case find (not . isAscii) str of 
    Nothing -> Right str
    Just ch -> Left $ NonAsciiChar ch

-- * Decoding

instance (Applicative f) => Decode f "r-ASCII" "r-ASCII" c str where
    decoding = decAnyR
    
instance (Char8Find str, RecreateErr f, Applicative f) => Validate f "r-ASCII" "r-ASCII" () str where
    validation = validR encASCII


-- tst = encodeFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- tst2 = encodeFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- tst3 = encodeFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString)
