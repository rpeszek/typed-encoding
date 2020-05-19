{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common /restriction/ "r-" instances
module Data.TypedEncoding.Instances.Restriction.Common where

import           Data.Word

-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL

import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Internal.Instances.Combinators
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> import qualified Data.Text as T



instance (IsStringR str) =>  Encode (Either EncodeEx) "r-Word8-decimal" "r-Word8-decimal" c str where
    encoding = encWord8Dec
instance (Applicative f) => Decode f "r-Word8-decimal" "r-Word8-decimal" c str where
    decoding = decAnyR
instance (IsStringR str) =>  Validate (Either RecreateEx) "r-Word8-decimal" "r-Word8-decimal" c str where
    validation = validR encWord8Dec

encWord8Dec :: (IsStringR str) => Encoding (Either EncodeEx) "r-Word8-decimal" "r-Word8-decimal" c str
encWord8Dec = mkEncoding $ implEncodeF @"r-Word8-decimal" (verifyWithRead @Word8 "Word8-decimal")


instance (IsStringR str) =>  Encode (Either EncodeEx) "r-Int-decimal" "r-Int-decimal" c str where
    encoding = encIntDec
instance (Applicative f) => Decode f "r-Int-decimal" "r-Int-decimal" c str where
    decoding = decAnyR
instance (IsStringR str) =>  Validate (Either RecreateEx) "r-Int-decimal" "r-Int-decimal" c str where
    validation = validR encIntDec

encIntDec :: (IsStringR str) => Encoding (Either EncodeEx) "r-Int-decimal" "r-Int-decimal" c str
encIntDec =  mkEncoding $ implEncodeF @"r-Int-decimal" (verifyWithRead @Int "Int-decimal")

instance (IsStringR str, RecreateErr f, Applicative f) => RecreateF f (Enc xs c str) (Enc ("r-Int-decimal" ': xs) c str) where
    checkPrevF = implCheckPrevF (asRecreateErr @"r-Int-decimal" . verifyWithRead @Int "Int-decimal")


-- tst :: T.Text
-- tst = fromString $ show $ (fromString $ "123" :: T.Text)
