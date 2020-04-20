{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Examples of how to work with encoded data.
-- This topic is (an interesting) work-in-progress.
--
-- Modifying encoded data would typically corrupt the encoding. 
-- Current approach is to use Unsafe wrapping class that exposes
-- Functor and (limited) Applicative and Monad instances.
module Examples.Unsafe where


import           Data.Proxy

import qualified Data.Text as T

import           Data.Char

import           Data.Encoding
import qualified Data.Encoding.Instances.ASCII as EnASCII
import qualified Data.Encoding.Unsafe as Unsafe

import           Data.Semigroup ((<>))


-- | Starting example
exAsciiE :: Either EnASCII.NonAsciiChar (Enc '["r-ASCII"] () T.Text)
exAsciiE = encodeFAll . toEncoding () $ "HELLO" 

-- | Example use of 'T.toLower' within encoded data
-- this operation is safe for ASCII restriction
-- but @Enc '["r-ASCII"] () T.Text@ does not expose it
-- We use Functor instance of Unsafe wrapper type to accomplish this
--
-- >>> exAsciiE
-- Right (MkEnc Proxy () "HELLO")
-- >>> exAsciiE >>= pure . Unsafe.withUnsafe (fmap T.toLower)
-- Right (MkEnc Proxy () "hello")
toLowerAscii :: Either EnASCII.NonAsciiChar (Enc '["r-ASCII"] () T.Text)
toLowerAscii = exAsciiE >>= pure . Unsafe.withUnsafe (fmap T.toLower)

-- | Example use of 'T.toLower' within encoded data
-- this operation is safe for ASCII restriction
-- but @Enc '["r-ASCII"] () T.Text@ does not expose it
-- We use Functor instance of Unsafe wrapper type to accomplish this
--
-- >>> let Right hELLO = exAsciiE
-- >>> let Right hello = toLowerAscii
-- >>> Unsafe.runUnsafe ((<>) <$> Unsafe.Unsafe hELLO <*> Unsafe.Unsafe hello)
-- MkEnc Proxy () "HELLOhello"
appendAscii :: Either EnASCII.NonAsciiChar (Enc '["r-ASCII"] () T.Text)
appendAscii = do 
    hELLO <- exAsciiE
    hello <- toLowerAscii
    pure $ Unsafe.runUnsafe ((<>) <$> Unsafe.Unsafe hELLO <*> Unsafe.Unsafe hello)


-- | Data.Encoding exports unsafeGetPayload
-- 
-- >>> exAsciiE >>= pure . unsafeGetPayload . Unsafe.withUnsafe (fmap T.length) 
-- Right 5
extractSize = exAsciiE >>= pure . unsafeGetPayload . Unsafe.withUnsafe (fmap T.length)   