
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Direct use of these methods is discouraged.
-- Use "Data.TypedEncoding.Instances.Support.Encode" or "Data.TypedEncoding.Instances.Support.Decode" 
module Data.TypedEncoding.Instances.Support.Unsafe where

import           Data.Proxy
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Combinators.Unsafe


-- |
-- @since 0.1.0.0
implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (const f)

-- |
-- @since 0.1.0.0
implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (UnsafeMkEnc _ conf str) = UnsafeMkEnc Proxy conf <$> f conf str

-- |
-- @since 0.1.0.0
implTranP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP f  = implTranF' (\c -> pure . f)

-- |
-- @since 0.1.0.0
implTranP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP' f  = implTranF' (\c -> pure . f c)

-- |
-- @since 0.2.1.0
implChangeAnn :: Functor f => (Enc enc1 conf str -> f (Enc enc2a conf str)) -> Enc enc1 conf str -> f (Enc enc2b conf str)
implChangeAnn fn = fmap (withUnsafeCoerce id) . fn

