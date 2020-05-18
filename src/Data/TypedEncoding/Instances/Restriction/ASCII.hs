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
-- >>> encFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- Right (MkEnc Proxy () "Hello World")
--
-- >>> encFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- Left (EncodeEx "r-ASCII" (NonAsciiChar '\194'))
module Data.TypedEncoding.Instances.Restriction.ASCII where

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Internal.Class.Util.StringConstraints

import           Data.TypedEncoding.Internal.Util (explainBool)
import           Data.Char


-- $setup
-- >>> :set -XDataKinds -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.ByteString as B
-- >>> import qualified Data.ByteString.Char8 as B8
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.ByteString()
-- >>> :{
-- instance Arbitrary (Enc '["r-ASCII"] () B.ByteString) where 
--      arbitrary =  fmap (unsafeSetPayload ()) 
--                   . flip suchThat (B8.all isAscii) 
--                        $ arbitrary 
-- :}
--


-----------------
-- Conversions --
-----------------

-- TODO v0.3 Superset

-- | allow to treat ASCII encodings as UTF8 forgetting about B64 encoding
-- 
-- UTF-8 is backward compatible on first 128 characters using just one byte to store it.
-- 
-- Payload does not change when @ASCII@ only strings are encoded to @UTF8@ in types like @ByteString@.
--
-- >>> let Right tstAscii = encFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- >>> displ (inject @ "r-UTF8" tstAscii)
-- "MkEnc '[r-UTF8] () (Text Hello World)"
instance Superset "r-UTF8" "r-ASCII" where

-- type instance IsSuperset "r-UTF8" "r-ASCII" = True  
-- type instance IsSuperset "r-ASCII" "r-ASCII" = True  

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
encASCIIChar = mkEncoding $ implEncodeF @"r-ASCII" (\c -> explainBool NonAsciiChar (c, isAscii c))    

encASCII :: Char8Find str =>  Encoding (Either EncodeEx) "r-ASCII" "r-ASCII" c str
encASCII = mkEncoding $ implEncodeF @"r-ASCII" encImpl

encImpl :: Char8Find str => str -> Either NonAsciiChar str
encImpl str = case find (not . isAscii) str of 
    Nothing -> Right str
    Just ch -> Left $ NonAsciiChar ch

-- * Decoding

instance (Applicative f) => Decode f "r-ASCII" "r-ASCII" c str where
    decoding = decAnyR
    

--- OLD 


instance (Char8Find str, RecreateErr f, Applicative f) => RecreateF f (Enc xs c str) (Enc ("r-ASCII" ': xs) c str) where
    checkPrevF = implCheckPrevF (asRecreateErr @"r-ASCII" . encImpl)


-- tst = encFAll . toEncoding () $ "Hello World" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- tst2 = encFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
-- tst3 = encFAll . toEncoding () $ "\194\160" :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString)
