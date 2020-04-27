{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Common restrictions "r-" instances
module Data.TypedEncoding.Instances.Restriction.Common where

import           Data.Word
import           Data.String
import           Text.Read
import           Data.Proxy

-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL

import           Data.TypedEncoding.Internal.Class.IsStringR
import           Data.TypedEncoding.Instances.Support

-- $setup
-- >>> import qualified Data.Text as T


prxyWord8Decimal =  Proxy :: Proxy "r-Word8-decimal"

instance (IsStringR str, IsString str) =>  EncodeF (Either EncodeEx) (Enc xs c str) (Enc ("r-Word8-decimal" ': xs) c str) where
    encodeF = implEncodeF prxyWord8Decimal (verifyWithRead (Proxy :: Proxy Word8) "Word8-decimal")
    -- encodeF = implEncodeF (Proxy :: Proxy "r-Word8-decimal") (verifyWithRead (Proxy :: Proxy Word8) "Word8-decimal")
instance (IsStringR str, IsString str, RecreateErr f, Applicative f) => RecreateF f (Enc xs c str) (Enc ("r-Word8-decimal" ': xs) c str) where
    checkPrevF = implCheckPrevF (asRecreateErr prxyWord8Decimal . verifyWithRead (Proxy :: Proxy Word8) "Word8-decimal")
instance (IsStringR str, IsString str, Applicative f) => DecodeF f (Enc ("r-Word8-decimal" ': xs) c str) (Enc xs c str) where
    decodeF = implTranP id 



-- | sometimes show . read is not identity, eg. Word8:
--
-- >>> read "256" :: Word8
-- 0
--
-- >>> verifyWithRead (Proxy :: Proxy Word8) "Word8-decimal" (T.pack "256")
-- Left "Payload does not satisfy format Word8-decimal: 256"
-- >>> verifyWithRead (Proxy :: Proxy Word8) "Word8-decimal" (T.pack "123")
-- Right "123"
verifyWithRead :: forall str a . (IsStringR str, IsString str, Read a, Show a) => Proxy a -> String -> str -> Either String str
verifyWithRead _ msg x = 
    let s = toString $ x
        a :: Maybe a = readMaybe s
        check = (show <$> a) == Just s 
    in if check
       then Right x
       else Left $ "Payload does not satisfy format " ++ msg ++ ": " ++ s    
        

-- tst :: T.Text
-- tst = fromString $ show $ (fromString $ "123" :: T.Text)