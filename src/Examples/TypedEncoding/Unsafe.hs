{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Examples about how to work with encoded data.
-- This topic is (an interesting) work-in-progress.
--
-- Modifying encoded data would typically corrupt the encoding. 
-- Current approach is to use 'Data.TypedEncoding.Unsafe.Unsafe' wrapping class that exposes
-- Functor and (limited) Applicative and Monad instances.

module Examples.TypedEncoding.Unsafe where


import           Data.Proxy

import qualified Data.Text as T

import           Data.Char

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Instances.ASCII as EnASCII
import qualified Data.TypedEncoding.Unsafe as Unsafe

import           Data.Semigroup ((<>))

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds

-- | Starting example
exAsciiTE :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
exAsciiTE = encodeFAll . toEncoding () $ "HELLO" 

-- | with either removed
exAsciiT :: Enc '["r-ASCII"] () T.Text
Right exAsciiT = exAsciiTE

-- * Safe and Slow approach

-- |
-- 'recreateFAll' is the way to recover encoding in a safe way
--
-- >>> let payload = getPayload exAsciiT
-- >>> let newPayload = payload <> " some extra stuff"
-- >>> recreateFAll . toEncoding () $ newPayload :: Either RecreateEx (Enc '["r-ASCII"] () T.Text)
-- Right (MkEnc Proxy () "HELLO some extra stuff")
--
modifiedAsciiT :: Either RecreateEx (Enc '["r-ASCII"] () T.Text)
modifiedAsciiT =  recreateFAll . toEncoding () . ( <> " some extra stuff") . getPayload $ exAsciiT
  

-- * Unsafe but fast

-- |
-- The issue with 'recreateFAll' is that it may be expensive.
--
-- This apprach uses 'Data.TypedEncoding.Unsafe.Unsafe' to perform (in general risky) operation on
-- the internal payload.
--  
-- >>> exAsciiTE
-- Right (MkEnc Proxy () "HELLO")
-- >>> exAsciiTE >>= pure . Unsafe.withUnsafe (fmap T.toLower)
-- Right (MkEnc Proxy () "hello")
--
-- Example uses of 'T.toLower' within encoded data
-- this operation is safe for ASCII restriction
-- but @Enc '["r-ASCII"] () T.Text@ does not expose it
-- We use Functor instance of Unsafe wrapper type to accomplish this
toLowerAscii :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
toLowerAscii = exAsciiTE >>= pure . Unsafe.withUnsafe (fmap T.toLower)

-- | 
-- Similar example uses applicative instance of 'Unsafe.Unsafe'
--
-- >>> let Right hELLO = exAsciiTE
-- >>> let Right hello = toLowerAscii
-- >>> Unsafe.runUnsafe ((<>) <$> Unsafe.Unsafe hELLO <*> Unsafe.Unsafe hello)
-- MkEnc Proxy () "HELLOhello"
appendAscii :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
appendAscii = do 
    hELLO <- exAsciiTE
    hello <- toLowerAscii
    pure $ Unsafe.runUnsafe ((<>) <$> Unsafe.Unsafe hELLO <*> Unsafe.Unsafe hello)

