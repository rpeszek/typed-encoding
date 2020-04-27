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

import           Data.String
import           Data.Proxy
import           GHC.TypeLits
import           Data.Char
import           Data.String
import           Data.Word


instance IsString str => ToEncString "r-()" str () where
    toEncString _ = MkEnc Proxy () (fromString "()")

instance IsString str => ToEncString "r-Int-decimal" str Int where
    toEncString i = MkEnc Proxy () (fromString . show $ i)

instance IsString str => ToEncString "r-Word8-decimal" str Word8 where
    toEncString i = MkEnc Proxy () (fromString . show $ i)
