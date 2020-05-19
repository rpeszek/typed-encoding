
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines some sample "do-" encodings
-- currently for example use only.
module Data.TypedEncoding.Instances.Do.Sample where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
import           Data.Char

import           Data.TypedEncoding.Instances.Support


instance Applicative f => Encode f "do-UPPER" "do-UPPER" c T.Text where
    encoding = mkEncoding (implEncodeP T.toUpper)

instance (RecreateErr f, Applicative f) => Validate f "do-UPPER" "do-UPPER" c T.Text where
    validation = mkValidation $
                          implCheckPrevF (asRecreateErr @"do-UPPER" . (\t -> 
                                 let (g,b) = T.partition isUpper t
                                 in if T.null b
                                    then Right t
                                    else Left $ "Found not upper case chars " ++ T.unpack b)
                           )


instance Applicative f => Encode f "do-UPPER" "do-UPPER" c TL.Text where
    encoding = mkEncoding (implEncodeP TL.toUpper)



instance Applicative f => Encode f "do-lower" "do-lower" c T.Text where
    encoding = mkEncoding $ implEncodeP T.toLower   

instance Applicative f => Encode f "do-lower" "do-lower" c TL.Text where
    encoding = mkEncoding $  implEncodeP TL.toLower 

instance Applicative f => Encode f "do-Title" "do-Title" c T.Text where
    encoding = mkEncoding $ implEncodeP T.toTitle   

instance Applicative f => Encode f "do-Title" "do-Title" c TL.Text where
    encoding = mkEncoding $ implEncodeP TL.toTitle   

instance Applicative f => Encode f "do-reverse" "do-reverse" c T.Text where
    encoding = mkEncoding $ implEncodeP T.reverse 
instance Applicative f => Encode f "do-reverse" "do-reverse" c TL.Text where
    encoding = mkEncoding $ implEncodeP TL.reverse    

newtype SizeLimit = SizeLimit {unSizeLimit :: Int} deriving (Eq, Show)
instance (HasA SizeLimit c, Applicative f) => Encode f "do-size-limit" "do-size-limit" c T.Text where
    encoding = mkEncoding $ implEncodeP' (T.take . unSizeLimit . has @ SizeLimit) 
instance (HasA SizeLimit c, Applicative f) => Encode f "do-size-limit" "do-size-limit" c B.ByteString where
    encoding = mkEncoding $ implEncodeP' (B.take . unSizeLimit .  has @ SizeLimit) 

