
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines some example "do-" encodings
-- 
-- See "Examples.TypedEncoding.Overview" for usage examples.
-- 
-- (moved from @Data.TypedEncoding.Instances.Do.Sample@)
--
-- @since 0.5.0.0
module Examples.TypedEncoding.Instances.Do.Sample where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B

import           Data.Char

import           Data.TypedEncoding.Instances.Support
import           Data.TypedEncoding.Instances.Support.Unsafe

-- |
-- @since 0.3.0.0 
instance Applicative f => Encode f "do-UPPER" "do-UPPER" c T.Text where
    encoding = _implEncodingP T.toUpper

instance (RecreateErr f, Applicative f) => Validate f "do-UPPER" "do-UPPER" c T.Text where
    validation = _mkValidation $
                          implTranF (asRecreateErr @"do-UPPER" . (\t -> 
                                 let (g,b) = T.partition isUpper t
                                 in if T.null b
                                    then Right t
                                    else Left $ "Found not upper case chars " ++ T.unpack b)
                           )

instance Applicative f => Encode f "do-UPPER" "do-UPPER" c TL.Text where
    encoding = _implEncodingP TL.toUpper


-- |
-- @since 0.3.0.0 
instance Applicative f => Encode f "do-lower" "do-lower" c T.Text where
    encoding = _implEncodingP T.toLower   

instance Applicative f => Encode f "do-lower" "do-lower" c TL.Text where
    encoding = _implEncodingP TL.toLower 

-- |
-- @since 0.3.0.0 
instance Applicative f => Encode f "do-Title" "do-Title" c T.Text where
    encoding = _implEncodingP T.toTitle   

instance Applicative f => Encode f "do-Title" "do-Title" c TL.Text where
    encoding = _implEncodingP TL.toTitle   

-- |
-- @since 0.3.0.0 
instance Applicative f => Encode f "do-reverse" "do-reverse" c T.Text where
    encoding = _implEncodingP T.reverse 
instance Applicative f => Encode f "do-reverse" "do-reverse" c TL.Text where
    encoding = _implEncodingP TL.reverse    

-- |
-- @since 0.1.0.0
newtype SizeLimit = SizeLimit {unSizeLimit :: Int} deriving (Eq, Show)

-- |
-- @since 0.3.0.0 
instance (HasA SizeLimit c, Applicative f) => Encode f "do-size-limit" "do-size-limit" c T.Text where
    encoding = _implEncodingConfP (T.take . unSizeLimit . has @ SizeLimit) 
instance (HasA SizeLimit c, Applicative f) => Encode f "do-size-limit" "do-size-limit" c B.ByteString where
    encoding = _implEncodingConfP (B.take . unSizeLimit .  has @ SizeLimit) 

