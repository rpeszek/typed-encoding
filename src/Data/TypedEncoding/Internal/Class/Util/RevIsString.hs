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
module Data.TypedEncoding.Internal.Class.Util.RevIsString () where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.String
import           Data.Proxy
-- import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy.Char8 as BL

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
-- @B.unpack $ B.pack "\160688" == "\176"@
--
-- This class is separated from 'RevIsStringIso' to allow instances from /smaller/ types
-- the can inject into the 'String' type.
class RevIsStringInj str where
    toString :: str -> String

prop_toStringFromString :: forall s . (IsString s, RevIsStringInj s) => Proxy s -> String -> Bool
prop_toStringFromString _ x = x == (toString @s . fromString $ x)


-- |
-- prop> prop_toStringFromString (Proxy :: Proxy T.Text) 
instance RevIsStringInj T.Text where
    toString = T.unpack    

-- |
-- prop> prop_toStringFromString (Proxy :: Proxy TL.Text) 
instance RevIsStringInj TL.Text where
    toString = TL.unpack  


-- will not work!
-- prop> prop_toStringFromString (Proxy :: Proxy B.ByteString) 
-- instance RevIsStringInj B.ByteString where
--     toString = B.unpack

-- will not work!
-- prop> prop_toStringFromString (Proxy :: Proxy BL.ByteString) 
-- instance RevIsStringInj BL.ByteString where
--     toString = BL.unpack


-- | Same as 'RevIsStringInj' but with additional
--
-- law for types that are also @IsString@:
-- @
--  fromString . toString == id
-- @
class RevIsStringInj str => RevIsStringIso str where

prop_fromStringToString :: forall s . (IsString s, RevIsStringIso s, Eq s) => s -> Bool
prop_fromStringToString x = x == (fromString @s . toString $ x)

-- |
-- prop> prop_fromStringToString @T.Text
instance RevIsStringIso T.Text where

-- |
-- prop> prop_fromStringToString @TL.Text
instance RevIsStringIso TL.Text where    