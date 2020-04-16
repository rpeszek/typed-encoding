
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Encoding.Instances.Encode.Sample where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


import           Data.Encoding.Internal.Types
import           Data.Encoding.Internal.Class
import           Data.Proxy
import           GHC.TypeLits
import qualified Data.List as L


instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("UPPER" ': xs) c T.Text) where
    encodeF = implTranP T.toUpper
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("UPPER" ': xs) c TL.Text) where
    encodeF = implTranP TL.toUpper 

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("lower" ': xs) c T.Text) where
    encodeF = implTranP T.toLower    
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("lower" ': xs) c TL.Text) where
    encodeF = implTranP TL.toLower 

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("Title" ': xs) c T.Text) where
    encodeF = implTranP T.toTitle   
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("Title" ': xs) c TL.Text) where
    encodeF = implTranP TL.toTitle   

instance Applicative f => EncodeF f (Enc xs c T.Text) (Enc ("reverse" ': xs) c T.Text) where
    encodeF = implTranP T.reverse 
instance Applicative f => EncodeF f (Enc xs c TL.Text) (Enc ("reverse" ': xs) c TL.Text) where
    encodeF = implTranP TL.reverse    

newtype SizeLimit = SizeLimit {unSizeLimit :: Int} deriving (Eq, Show)
instance (HasA c SizeLimit, Applicative f) => EncodeF f (Enc xs c T.Text) (Enc ("size-limit" ': xs) c T.Text) where
    encodeF =  implTranP' (T.take . unSizeLimit . has (Proxy :: Proxy SizeLimit)) 

