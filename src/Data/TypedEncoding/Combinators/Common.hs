
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Common encoding combinators.
-- This module is re-exported in Data.TypedEncoding
module Data.TypedEncoding.Combinators.Common where

import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Combinators.Unsafe
import           Data.TypedEncoding.Common.Class.Util (Append)
import           GHC.TypeLits
import           Data.Proxy

-- $setup
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.Word


-- * Partial application of encoding / decoding / recreation

-- | Any valid transformation of encodings (encoding / decoding / recreation) can be 
-- replayed on top of another encoding stack. 
--
-- This subsumes various /encodePart, decodePart, recreatePart/ combinators.
--
-- @since 0.3.0.0
aboveF :: forall (ts :: [Symbol]) xs ys f c str . (Functor f) =>
           (Enc xs c str -> f (Enc ys c str)) 
           -> Enc (Append xs ts) c str -> f (Enc (Append ys ts) c str)
aboveF fn (UnsafeMkEnc _ conf str) = 
    let re :: f (Enc ys c str) = fn $ UnsafeMkEnc Proxy conf str
    in  UnsafeMkEnc Proxy conf . getPayload <$> re

-- |
-- @since 0.3.0.0
above :: forall (ts :: [Symbol]) xs ys c str . 
           (Enc xs c str -> Enc ys c str) 
           -> Enc (Append xs ts) c str -> Enc (Append ys ts) c str
above fn (UnsafeMkEnc _ conf str) = 
    let re ::Enc ys c str = fn $ UnsafeMkEnc Proxy conf str
    in  UnsafeMkEnc Proxy conf . getPayload $ re


-- * Other 


-- |
-- @since 0.3.0.0
getTransformF :: forall e1 e2 f c s1 s2 . Functor f => (Enc e1 c s1 -> f (Enc e2 c s2)) -> c -> s1 -> f s2
getTransformF fn c str = getPayload <$> fn (unsafeSetPayload c str)


