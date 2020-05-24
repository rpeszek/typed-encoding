
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | Exports for instance creation.
-- 
-- Contains typical things needed when implementing
-- encoding, decoding, recreate, or type to string conversions.
module Data.TypedEncoding.Instances.Support.Common where

import           Data.TypedEncoding.Instances.Support.Unsafe
import           Data.TypedEncoding.Common.Types
import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications


-- * Decoding 

-- | Universal decoding for all "r-" types
--
-- @since 0.3.0.0
decAnyR :: forall r f c str . (Restriction r, Applicative f) => Decoding f r r c str
decAnyR = decAnyR' @r @r

-- |
-- @since 0.3.0.0
decAnyR' :: forall alg r f c str . (Restriction r, Applicative f) => Decoding f r alg c str
decAnyR' = UnsafeMkDecoding Proxy (implTranP id) 

-- |
-- @since 0.3.0.0
decAnyR_ :: forall r f c str alg . (Restriction r, Algorithm r alg, Applicative f) => Decoding f r alg c str
decAnyR_ = mkDecoding $ implTranP id

