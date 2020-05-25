{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 
-- Checks that satisfy D76 Unicode standard (/text/ replaces chars that are in range
-- U+D800 to U+DFFF inclusive)
--
-- Note, no IsSuperset "r-UNICODE.D76" "r-CHAR8" mapping even though the numeric range of D76 includes all CHAR8 bytes.
-- This is more 'nominal' decision that prevents certain unwanted conversions from being possible.
--
-- Similarly no IsSuperset "r-UNICODE.D76" "r-ByteRep", this annotation acts as a guard to what can go into @Text@.
-- 
-- @since 0.4.0.0
module Data.TypedEncoding.Instances.Restriction.D76 where

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Common.Class.Util.StringConstraints

import           Data.TypedEncoding.Internal.Util (explainBool)
import           Data.Char


-- $setup
-- >>> :set -XDataKinds -XTypeApplications


-----------------
-- Encodings  --
-----------------

newtype NonTextChar = NonTextChar Char deriving (Eq, Show)

-- * Encoding @"r-UNICODE.D76"@

instance Encode (Either EncodeEx) "r-UNICODE.D76" "r-UNICODE.D76" c Char where
    encoding = encD76Char    

instance Encode (Either EncodeEx) "r-UNICODE.D76" "r-UNICODE.D76" c String where
    encoding = encD76

encD76Char :: Encoding (Either EncodeEx) "r-UNICODE.D76" "r-UNICODE.D76" c Char 
encD76Char = _implEncodingEx (\c -> explainBool NonTextChar (c, nonTextChar c))    

encD76 :: Encoding (Either EncodeEx) "r-UNICODE.D76" "r-UNICODE.D76" c String
encD76 = _implEncodingEx @"r-UNICODE.D76" encImpl

-- | No-check version
trustMe :: Applicative f =>  Encoding f "r-UNICODE.D76" "r-UNICODE.D76" c String
trustMe = _implEncodingP id



-- * Decoding @"r-UNICODE.D76"@

instance (Applicative f) => Decode f "r-UNICODE.D76" "r-UNICODE.D76" c str where
    decoding = decAnyR
    
instance (RecreateErr f, Applicative f) => Validate f "r-UNICODE.D76" "r-UNICODE.D76" () String where
    validation = validR encD76


-- * Implementation 

-- @'\xd800'@ to  @'\xdfff'@ inclusive
nonTextChar :: Char -> Bool
nonTextChar c =  x >= 55296 && x <= 57343
   where x = ord c


-- \ UNICODE.D76
encImpl :: String -> Either NonTextChar String
encImpl str = case find nonTextChar str of 
    Nothing -> Right str
    Just ch -> Left $ NonTextChar ch

