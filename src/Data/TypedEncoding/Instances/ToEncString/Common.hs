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

-- | Common 'ToEncString' and 'FromEncString' instances.
module Data.TypedEncoding.Instances.ToEncString.Common where


-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.ByteString as B
-- -- import qualified Data.ByteString.Lazy as BL

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Internal.Class.IsStringR

import           Data.String
import           Data.Proxy
import           Data.Word
import           Text.Read
import           Data.Functor.Identity

-- f nm ann a str
instance IsString str => ToEncString Identity "r-()" "r-()" () str where
    toEncF _ = Identity $ MkEnc Proxy () (fromString "()")

instance IsString str => ToEncString Identity "r-Int-decimal" "r-Int-decimal" Int str where
    toEncF  i = Identity $ MkEnc Proxy () (fromString . show $ i)

instance IsString str => ToEncString Identity "r-Word8-decimal" "r-Word8-decimal" Word8 str where
    toEncF  i = Identity $ MkEnc Proxy () (fromString . show $ i)

-- All instances of "r-Word8-decimal" are  @Show@ / @Read@ based
instance (IsStringR str, UnexpectedDecodeErr f, Applicative f) => FromEncString f "r-Word8-decimal" "r-Word8-decimal" Word8 str where
    fromEncF  = asUnexpected @ "r-Word8-decimal" . readEither . toString . getPayload
