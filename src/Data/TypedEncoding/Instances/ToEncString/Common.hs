{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.TypedEncoding.Instances.ToEncString.Common where


-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.ByteString as B
-- -- import qualified Data.ByteString.Lazy as BL

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Internal.Class.IsStringR

import           Data.String
import           Data.Proxy
import           GHC.TypeLits
import           Data.Char
import           Data.String
import           Data.Word
import           Text.Read
import           Data.Functor.Identity


instance IsString str => ToEncString "r-()" str Identity () where
    toEncStringF _ = Identity $ MkEnc Proxy () (fromString "()")

instance IsString str => ToEncString "r-Int-decimal" str Identity Int where
    toEncStringF i = Identity $ MkEnc Proxy () (fromString . show $ i)

instance IsString str => ToEncString "r-Word8-decimal" str Identity Word8 where
    toEncStringF i = Identity $ MkEnc Proxy () (fromString . show $ i)

instance (IsStringR str, UnexpectedDecodeErr f, Applicative f) => FromEncString Word8 f str "r-Word8-decimal" where
    fromEncStringF  = asUnexpected_ @ "r-Word8-decimal" . readEither . toString . getPayload
