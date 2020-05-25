{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 
-- @"r-ByteRep"@ represents Characters used for Byte representations.
--
-- It is common to use @Char@ instead of @Word8@ when low level programming on @ByteString@.
--
-- This annotation represents such use of string characters.
-- 
-- Checks if all chars are @< \'\256\'@
--
-- Currently, this should be not used as superset 'Data.TypedEncoding.Common.Class.Superset.IsSuperset'
--
-- It is subset of "r-CHAR8":
--
-- @Superset "r-CHAR8" ""r-ByteRep"    
--
-- Currently, is (intentionally not decodable)
--
-- @since 0.3.1.0
module Data.TypedEncoding.Instances.Restriction.ByteRep where

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Common.Class.Util.StringConstraints

import           Data.TypedEncoding.Internal.Util (explainBool)
import           Data.Char

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

-- $setup
-- >>> :set -XDataKinds -XTypeApplications


-----------------
-- Encodings  --
-----------------

data CharOutOfRange = CharOutOfRange Int Char deriving (Eq, Show)

-- * Encoding @"r-ByteRep"@

instance Encode (Either EncodeEx) "r-ByteRep" "r-ByteRep" c Char where
    encoding = encByteChar    

instance Encode (Either EncodeEx) "r-ByteRep" "r-ByteRep" c B.ByteString where
    encoding = encByteRepB

instance Encode (Either EncodeEx) "r-ByteRep" "r-ByteRep" c BL.ByteString where
    encoding = encByteRepBL

instance Encode (Either EncodeEx) "r-ByteRep" "r-ByteRep" c String where
    encoding = encByteRepS

encByteChar :: Encoding (Either EncodeEx) "r-ByteRep" "r-ByteRep" c Char 
encByteChar = _implEncodingEx (\c -> explainBool (CharOutOfRange 255) (c, (> 255) . ord $ c))    

encByteRepB :: Encoding (Either EncodeEx) "r-ByteRep" "r-ByteRep" c B.ByteString
encByteRepB = _implEncodingEx @"r-ByteRep" (encImpl 255)

encByteRepBL :: Encoding (Either EncodeEx) "r-ByteRep" "r-ByteRep" c BL.ByteString
encByteRepBL = _implEncodingEx @"r-ByteRep" (encImpl 255)

encByteRepS :: Encoding (Either EncodeEx) "r-ByteRep" "r-ByteRep" c String
encByteRepS = _implEncodingEx @"r-ByteRep" (encImpl 255)


-- * Decoding @"r-ByteRep"@

-- instance (Applicative f) => Decode f "r-ByteRep" "r-ByteRep" c str where
--     decoding = decAnyR


instance (RecreateErr f, Applicative f) => Validate f "r-ByteRep" "r-ByteRep" () B.ByteString where
    validation = validR encByteRepB

instance (RecreateErr f, Applicative f) => Validate f "r-ByteRep" "r-ByteRep" () BL.ByteString where
    validation = validR encByteRepBL

instance (RecreateErr f, Applicative f) => Validate f "r-ByteRep" "r-ByteRep" () String where
    validation = validR encByteRepS


-- * Implementation 

-- 255 for CHAR8 / "r-ByteRep"
encImpl :: Char8Find str => Int -> str -> Either CharOutOfRange str
encImpl bound str = case find ((> bound) . ord) str of 
    Nothing -> Right str
    Just ch -> Left $ CharOutOfRange bound ch

