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
import           Data.String

-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL

import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Internal.Instances.Combinators
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> import qualified Data.Text as T


instance (IsStringR str, IsString str) =>  EncodeF (Either EncodeEx) (Enc xs c str) (Enc ("r-Word8-decimal" ': xs) c str) where
    encodeF = implEncodeF @"r-Word8-decimal" (verifyWithRead @Word8 "Word8-decimal")
instance (IsStringR str, IsString str, RecreateErr f, Applicative f) => RecreateF f (Enc xs c str) (Enc ("r-Word8-decimal" ': xs) c str) where
    checkPrevF = implCheckPrevF (asRecreateErr @"r-Word8-decimal" . verifyWithRead @Word8 "Word8-decimal")
instance (IsStringR str, IsString str, Applicative f) => DecodeF f (Enc ("r-Word8-decimal" ': xs) c str) (Enc xs c str) where
    decodeF = implTranP id 



-- tst :: T.Text
-- tst = fromString $ show $ (fromString $ "123" :: T.Text)
