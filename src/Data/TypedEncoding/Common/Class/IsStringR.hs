

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This Module will be removed in 0.3.x.x in favor of 
-- "Data.TypedEncoding.Common.Class.Util.StringConstraints"
module Data.TypedEncoding.Common.Class.IsStringR where

import           Data.Proxy

import           Data.String
-- import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL



-- $setup
-- >>> :set -XScopedTypeVariables -XTypeApplications -XAllowAmbiguousTypes
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.Instances.Text()
-- >>> import Test.QuickCheck.Instances.ByteString()

-- | This class will be removed in 0.3.x.x in favor of classes definined in 
-- "Data.TypedEncoding.Common.Class.Util.StringConstraints"
--
-- Reverses 'Data.String.IsString'
--
-- laws:
--
-- @
--  toString . fromString == id
--  fromString . toString == id
-- @
--
-- Note: ByteString is not a valid instance, ByteString "r-ASCII", or "r-UTF8" would
-- be needed.
-- @B.unpack $ B.pack "\160688" == "\176"@
class IsStringR a where
    toString :: a -> String

prop_fromStringToString :: forall s . (IsString s, IsStringR s, Eq s) => s -> Bool
prop_fromStringToString x = x == (fromString @s . toString $ x)

prop_toStringFromString :: forall s . (IsString s, IsStringR s) => Proxy s -> String -> Bool
prop_toStringFromString _ x = x == (toString @s . fromString $ x)

-- This would not work
-- @B.unpack $ B.pack "\160688" == "\176"@
--
-- prop> prop_toStringFromString (Proxy :: Proxy B.ByteString) 
-- prop> prop_fromStringToString @B.ByteString
-- instance IsStringR B.ByteString where
--     toString = B.unpack
 

-- |
-- prop> prop_toStringFromString (Proxy :: Proxy T.Text) 
-- prop> prop_fromStringToString @T.Text
instance IsStringR T.Text where
    toString = T.unpack    

-- |
-- prop> prop_toStringFromString (Proxy :: Proxy TL.Text) 
-- prop> prop_fromStringToString @TL.Text
instance IsStringR TL.Text where
    toString = TL.unpack  

instance IsStringR [Char] where
    toString = id
