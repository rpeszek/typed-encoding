{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 
--
-- "r-CHAR8" should not be used directly, only as superset.
--
-- This module includes tests that verify that all chars are @< \'\256\'@
--
-- Encoding functions are here for test support only, no instances.
--
-- @since 0.3.1.0
module Data.TypedEncoding.Instances.Restriction.CHAR8 where

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Common.Class.Util.StringConstraints
import           Data.TypedEncoding.Instances.Restriction.ByteRep (encImpl, CharOutOfRange (..))

import           Data.TypedEncoding.Internal.Util (explainBool)
import           Data.Char


-- $setup
-- >>> :set -XDataKinds -XTypeApplications


-----------------
-- Test Encodings  --
-----------------

testEncChar8Char :: Encoding (Either EncodeEx) "r-CHAR8" "r-CHAR8" c Char 
testEncChar8Char = _implEncodingEx (\c -> explainBool (CharOutOfRange 255) (c, (> 255) . ord $ c))    

testEncCHAR8 :: Char8Find str =>  Encoding (Either EncodeEx) "r-CHAR8" "r-CHAR8" c str
testEncCHAR8 = _implEncodingEx @"r-CHAR8" (encImpl 255)


