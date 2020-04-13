
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Encoding.Instances.Encode.Simple where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL


import           Data.Encoding.Internal.Types
import           Data.Encoding.Internal.Class
import           Data.Proxy
import           GHC.TypeLits
import qualified Data.List as L


instance Encode (Enc xs c T.Text) (Enc ("UPPER" ': xs) c T.Text) where
    encode = implTran T.toUpper
instance Encode (Enc xs c TL.Text) (Enc ("UPPER" ': xs) c TL.Text) where
    encode = implTran TL.toUpper 

instance Encode (Enc xs c T.Text) (Enc ("lower" ': xs) c T.Text) where
    encode = implTran T.toLower    
instance Encode (Enc xs c TL.Text) (Enc ("lower" ': xs) c TL.Text) where
    encode = implTran TL.toLower 

instance Encode (Enc xs c T.Text) (Enc ("Title" ': xs) c T.Text) where
    encode = implTran T.toTitle   
instance Encode (Enc xs c TL.Text) (Enc ("Title" ': xs) c TL.Text) where
    encode = implTran TL.toTitle   

instance Encode (Enc xs c T.Text) (Enc ("reverse" ': xs) c T.Text) where
    encode = implTran T.reverse 
instance Encode (Enc xs c TL.Text) (Enc ("reverse" ': xs) c TL.Text) where
    encode = implTran TL.reverse    

newtype SizeLimit = SizeLimit {unSizeLimit :: Int} deriving (Eq, Show)
instance HasA c SizeLimit => Encode (Enc xs c T.Text) (Enc ("size-limit" ': xs) c T.Text) where
    encode =  implTran' (T.take . unSizeLimit . has (Proxy :: Proxy SizeLimit)) 

