{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TypedEncoding.Combinators.Unsafe where

import           Data.TypedEncoding.Internal.Enc
import           Data.Proxy

unsafeSetPayload :: conf -> str -> Enc enc conf str 
unsafeSetPayload  = MkEnc Proxy 

withUnsafeCoerce ::  (s1 -> s2) -> Enc e1 c s1 -> Enc e2 c s2
withUnsafeCoerce f (MkEnc _ conf str)  = MkEnc Proxy conf (f str)

withUnsafeCoerceF :: forall e1 e2 f c s1 s2 . Functor f => (s1 -> f s2) -> Enc e1 c s1 -> f (Enc e2 c s2)
withUnsafeCoerceF f (MkEnc _ conf str)  = MkEnc Proxy conf <$> f str

unsafeChangePayload ::  (s1 -> s2) -> Enc e c s1 -> Enc e c s2
unsafeChangePayload f (MkEnc p conf str)  = MkEnc p conf (f str) 
