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

-- | Future replacement for "Data.TypedEncoding.Internal.Class.IsStringR"
module Data.TypedEncoding.Internal.Class.Util.StringConstraints () where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.String
import           Data.Proxy
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

-- $setup
-- >>> :set -XScopedTypeVariables -XTypeApplications -XAllowAmbiguousTypes
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()


-- * IsString reversal

-- | Reverses 'Data.String.IsString'
--
-- law for types that are also @IsString@:
-- 
-- @
--  toString . fromString == id
-- @
--
-- Note: ByteString is not a valid instance, ByteString "r-ASCII", or "r-UTF8" would
-- be needed.
-- @B8.unpack $ B8.pack "\160688" == "\176"@
--
-- This class is separated from @ToStrIso@ to allow instances from /smaller/ types
-- the can inject into the 'String' type.
class ToStrInj str from where
    toString :: from -> str

prop_toStringFromString :: forall s . (IsString s, ToStrInj String s) => Proxy s -> String -> Bool
prop_toStringFromString _ x = x == (toString @String @s . fromString $ x)


-- |
-- prop> prop_toStringFromString (Proxy :: Proxy T.Text) 
instance ToStrInj String T.Text where
    toString = T.unpack    

-- |
-- prop> prop_toStringFromString (Proxy :: Proxy TL.Text) 
instance ToStrInj String TL.Text where
    toString = TL.unpack  


instance ToStrInj String String where
    toString = id

-- will not work!
-- prop> prop_toStringFromString (Proxy :: Proxy B.ByteString) 
-- instance ToStrInj String B.ByteString where
--     toString = B8.unpack

-- will not work!
-- prop> prop_toStringFromString (Proxy :: Proxy BL.ByteString) 
-- instance ToStrInj String BL.ByteString where
--     toString = BL8.unpack


-- | Same as @ToStrInj@ but with additional
--
-- law for types that are also @IsString@:
-- @
--  fromString . toString == id
-- @
class ToStrInj str from => ToStrIso str from where

prop_fromStringToString :: forall s . (IsString s, ToStrIso String s, Eq s) => s -> Bool
prop_fromStringToString x = x == (fromString @s . toString $ x)

-- |
-- prop> prop_fromStringToString @T.Text
instance ToStrIso String T.Text where

-- |
-- prop> prop_fromStringToString @TL.Text
instance ToStrIso String TL.Text where    

instance ToStrIso String String where

-- |
-- Used to find exceptions that violated "r-" encoding
-- Expected to be used to check encoding of ASCII-7 so Text and ByteString are compatible.
class Char7Find str where
    find :: (Char -> Bool) -> str -> Maybe Char


instance Char7Find T.Text where
    find = T.find

instance Char7Find TL.Text where
    find = TL.find    

-- |
-- B8.pack is basically a convenient way to get Word8 elements into ByteString.
--
-- During B8.pack conversion charters are downsized to the 0-255 range (become a Word8).
-- The length is preserved. 
-- 
-- >>> B8.pack "\160582"
-- "F"
--
-- >>> B.length $ B8.pack "\160582"
-- 1
--
-- This instance allows to check elementes of ByteString interpreting them as Char.
-- 
-- This may or may not work with UTF8 conversions.
-- Safe if restricting to 7bit code points.s
instance Char7Find B.ByteString where
    find = B8.find   

instance Char7Find BL.ByteString where
    find = BL8.find         