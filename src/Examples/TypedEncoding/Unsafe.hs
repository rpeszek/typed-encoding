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

import qualified Data.Text as T
import           Data.Semigroup ((<>))

import           Data.TypedEncoding
import qualified Data.TypedEncoding.Unsafe as Unsafe
import qualified Data.TypedEncoding.Instances.Restriction.ASCII()


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
-- Right (UnsafeMkEnc Proxy () "HELLO some extra stuff")
--
-- Alternatively, 'UncheckedEnc' type can be used in recreation, see 'Examples.TypedEncoding.Overview'
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
-- Right (UnsafeMkEnc Proxy () "HELLO")
-- >>> exAsciiTE >>= pure . Unsafe.withUnsafe (fmap T.toLower)
-- Right (UnsafeMkEnc Proxy () "hello")
--
-- Example uses of 'T.toLower' within encoded data
-- this operation is safe for ASCII restriction
-- but @Enc '["r-ASCII"] () T.Text@ does not expose it
-- We use Functor instance of Unsafe wrapper type to accomplish this
toLowerAscii :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
toLowerAscii = Unsafe.withUnsafe (fmap T.toLower) <$> exAsciiTE

-- | 
-- Similar example uses applicative instance of 'Unsafe.Unsafe'
--
-- >>> let Right hELLO = exAsciiTE
-- >>> let Right hello = toLowerAscii
-- >>> displ $ Unsafe.runUnsafe ((<>) <$> Unsafe.Unsafe hELLO <*> Unsafe.Unsafe hello)
-- "Enc '[r-ASCII] () (Text HELLOhello)"
appendAscii :: Either EncodeEx (Enc '["r-ASCII"] () T.Text)
appendAscii = do 
    hELLO <- exAsciiTE
    hello <- toLowerAscii
    pure $ Unsafe.runUnsafe ((<>) <$> Unsafe.Unsafe hELLO <*> Unsafe.Unsafe hello)

