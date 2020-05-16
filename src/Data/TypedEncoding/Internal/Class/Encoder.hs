{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- |
-- Internal definition of types
--
-- Possible replacement for EncodeFAll class that works with open definitions such as "r-ban"

module Data.TypedEncoding.Internal.Class.Encoder where

import           Data.TypedEncoding.Internal.Types.Enc
-- import           Data.TypedEncoding.Internal.Class.Util
import           Data.TypedEncoding.Internal.Class.Encode
import           Data.TypedEncoding.Internal.Util.TypeLits
import           GHC.TypeLits
-- import           Data.Symbol.Ascii


data Encoder f (enc :: [Symbol]) (grps :: [Symbol]) conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    ZeroEnc :: Encoder f '[] '[] conf str
    AppendEnc ::  (Enc xs conf str -> f (Enc (x ': xs) conf str)) -> Encoder f xs grps conf str -> Encoder f (x ': xs) ((TakeUntil x ":") ': grps) conf str

runEncoder :: forall grps enc f c str . (Monad f) => Encoder f enc grps c str -> Enc ('[]::[Symbol]) c str -> f (Enc enc c str)
runEncoder ZeroEnc enc0 = pure enc0
runEncoder (AppendEnc fn enc) enc0 = 
        let re :: f (Enc _ c str) = runEncoder enc enc0
        in re >>= fn

encodeFEncoder :: forall f t tg xs gxs c str . (tg ~ TakeUntil t ":", Encodings f xs gxs c str, EncodeF f (Enc xs c str) (Enc (t ': xs) c str)) => Encoder f (t ': xs) (tg ': gxs) c str
encodeFEncoder = AppendEnc (encodeF @f @(Enc xs c str) @(Enc (t ': xs) c str)) encodings

class Encodings f (enc :: [Symbol]) (grps :: [Symbol]) c str where
    encodings :: Encoder f enc grps c str

instance Encodings f '[] '[] c str where
    encodings = ZeroEnc 
