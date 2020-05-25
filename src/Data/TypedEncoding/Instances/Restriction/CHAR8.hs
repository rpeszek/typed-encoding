{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 
-- Checks if all chars are @< \'\256\'@
--
-- Should not be used directly, only as superset
--
-- Encoding functions are here for test support only, no instances
--
-- @since 0.3.1.0
module Data.TypedEncoding.Instances.Restriction.CHAR8 where

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Common.Class.Util.StringConstraints

import           Data.TypedEncoding.Internal.Util (explainBool)
import           Data.Char


-- $setup
-- >>> :set -XDataKinds -XTypeApplications


-----------------
-- Encodings  --
-----------------

data CharOutOfRange = CharOutOfRange Int Char deriving (Eq, Show)

-- * Encoding @"r-CHAR8"@

-- instance Encode (Either EncodeEx) "r-CHAR8" "r-CHAR8" c Char where
--     encoding = encChar8Char    

-- instance CharFind str => Encode (Either EncodeEx) "r-CHAR8" "r-CHAR8" c str where
--     encoding = encCHAR8

encChar8Char :: Encoding (Either EncodeEx) "r-CHAR8" "r-CHAR8" c Char 
encChar8Char = _implEncodingEx (\c -> explainBool (CharOutOfRange 255) (c, (> 255) . ord $ c))    

encCHAR8 :: CharFind str =>  Encoding (Either EncodeEx) "r-CHAR8" "r-CHAR8" c str
encCHAR8 = _implEncodingEx @"r-CHAR8" (encImpl 255)


-- * Decoding @"r-CHAR8"@

-- instance (Applicative f) => Decode f "r-CHAR8" "r-CHAR8" c str where
--     decoding = decAnyR
    
-- instance (CharFind str, RecreateErr f, Applicative f) => Validate f "r-CHAR8" "r-CHAR8" () str where
--     validation = validR encCHAR8


-- * Implementation 

-- 255 for CHAR8
encImpl :: CharFind str => Int -> str -> Either CharOutOfRange str
encImpl bound str = case findChar ((> bound) . ord) str of 
    Nothing -> Right str
    Just ch -> Left $ CharOutOfRange bound ch

