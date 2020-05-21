
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

import           Data.TypedEncoding.Instances.Support.Deprecated
import           Data.TypedEncoding.Common.Types
import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications


-- * Decoding 

-- | Universal decoding for all "r-" types
decAnyR :: forall r f c str . (Restriction r, Applicative f) => Decoding f r r c str
decAnyR = decAnyR' @r @r

decAnyR' :: forall alg r f c str . (Restriction r, Applicative f) => Decoding f r alg c str
decAnyR' = UnsafeMkDecoding Proxy (implTranP id) 

decAnyR_ :: forall r f c str alg . (Restriction r, Algorithm r alg, Applicative f) => Decoding f r alg c str
decAnyR_ = mkDecoding $ implTranP id

