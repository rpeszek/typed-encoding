{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE UndecidableInstances #-}


module Examples.TypedEncoding.ToEncString where

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.Support as EnT
import           Data.TypedEncoding.Instances.Restriction.Common
import           Data.TypedEncoding.Instances.ToEncString.Common

import           Data.Word
import           Data.Functor.Identity
import qualified Data.Text as T
import           GHC.TypeLits
import           Data.Proxy

-- $setup
-- >>> :set -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XFlexibleInstances -XTypeApplications

type IpV4 = IpV4F Word8

data IpV4F a = IpV4F {
     oct1 :: a
     , oct2 :: a
     , oct3 :: a
     , oct4 :: a
   } deriving (Show, Functor, Foldable)

tstIp :: IpV4
tstIp = IpV4F 128 1 1 10

instance Displ a => Displ (IpV4F a) where
    displ = show . fmap displ


-- |
-- >>> toEncString @"r-IPv4" @T.Text tstIp
-- MkEnc Proxy () "128.1.1.10"
instance ToEncString "r-IPv4" T.Text Identity IpV4 where
    toEncStringF = Identity . reduce . map
      where map :: IpV4F Word8 -> IpV4F (Enc '["r-Word8-decimal"] () T.Text) 
            map = fmap toEncString

            reduce :: IpV4F (Enc '["r-Word8-decimal"] () T.Text) -> (Enc '["r-IPv4"] () T.Text) 
            reduce = EnT.foldEncStr () (\a b-> if b == "" then a else a <> "." <> b) 

-- |
-- >>> let enc = toEncString @"r-IPv4" @T.Text tstIp
-- >>> fromEncString @IpV4 enc
-- IpV4F {oct1 = 128, oct2 = 1, oct3 = 1, oct4 = 10}

instance (UnexpectedDecodeErr f, Applicative f) => FromEncString IpV4 f T.Text "r-IPv4" where   
    fromEncStringF = fmap map . unreduce
      where unreduce :: Enc '["r-IPv4"] () T.Text -> f (IpV4F (Enc '["r-Word8-decimal"] () T.Text))
            unreduce = asUnexpected_ @"r-IPv4" . recover . EnT.splitPayload @ '["r-Word8-decimal"] (T.splitOn ".")
            
            map :: IpV4F (Enc '["r-Word8-decimal"] () T.Text) -> IpV4F Word8 
            map = fmap fromEncString

            recover ::  Show a => [a] -> Either String (IpV4F a)
            recover [o1,o2,o3,o4] = pure $ IpV4F o1 o2 o3 o4
            recover x = Left $ "Invalid Content" ++ show x
