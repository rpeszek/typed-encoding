
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Combinators reexported in Data.TypedEncoding
module Data.TypedEncoding.Combinators.Common where

import           Data.TypedEncoding.Common.Types
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
-- This subsumes various /encPart, decPart, recrPart/ combinators.
aboveF :: forall (ts :: [Symbol]) xs ys f c str . (Functor f) =>
           (Enc xs c str -> f (Enc ys c str)) 
           -> Enc (Append xs ts) c str -> f (Enc (Append ys ts) c str)
aboveF fn (MkEnc _ conf str) = 
    let re :: f (Enc ys c str) = fn $ MkEnc Proxy conf str
    in  MkEnc Proxy conf . getPayload <$> re


above :: forall (ts :: [Symbol]) xs ys c str . 
           (Enc xs c str -> Enc ys c str) 
           -> Enc (Append xs ts) c str -> Enc (Append ys ts) c str
above fn (MkEnc _ conf str) = 
    let re ::Enc ys c str = fn $ MkEnc Proxy conf str
    in  MkEnc Proxy conf . getPayload $ re

-- * Converting 'UncheckedEnc' to 'Enc'

