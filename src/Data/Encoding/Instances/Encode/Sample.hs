
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines some sample "do-" encodings
-- currently for example use only.
module Data.Encoding.Instances.Encode.Sample where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL

import           Data.Encoding.Instances.Support

import           Data.Proxy
import           GHC.TypeLits
import           Data.Char


instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("do-UPPER" ': xs) c T.Text) where
    encodeF = implTranP T.toUpper
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c T.Text) (Enc ("do-UPPER" ': xs) c T.Text) where
    checkPrevF = implTranF (asRecreateErr . (\t -> 
                                 let (g,b) = T.partition isUpper t
                                 in if T.null b
                                    then Right t
                                    else Left $ "Found not upper case chars " ++ T.unpack b)
                           )
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("do-UPPER" ': xs) c TL.Text) where
    encodeF = implTranP TL.toUpper 

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("do-lower" ': xs) c T.Text) where
    encodeF = implTranP T.toLower    
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("do-lower" ': xs) c TL.Text) where
    encodeF = implTranP TL.toLower 

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("do-Title" ': xs) c T.Text) where
    encodeF = implTranP T.toTitle   
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("do-Title" ': xs) c TL.Text) where
    encodeF = implTranP TL.toTitle   

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("do-reverse" ': xs) c T.Text) where
    encodeF = implTranP T.reverse 
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("do-reverse" ': xs) c TL.Text) where
    encodeF = implTranP TL.reverse    

newtype SizeLimit = SizeLimit {unSizeLimit :: Int} deriving (Eq, Show)
instance (HasA c SizeLimit, Applicative f) => EncodeF f (Enc xs c T.Text) (Enc ("do-size-limit" ': xs) c T.Text) where
    encodeF =  implTranP' (T.take . unSizeLimit . has (Proxy :: Proxy SizeLimit)) 
instance (HasA c SizeLimit, Applicative f) => EncodeF f (Enc xs c B.ByteString) (Enc ("do-size-limit" ': xs) c B.ByteString) where
    encodeF =  implTranP' (B.take . unSizeLimit . has (Proxy :: Proxy SizeLimit)) 

