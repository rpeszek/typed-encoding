{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Basic unsafe operations on 'Enc'
module Data.TypedEncoding.Combinators.Unsafe where

import           Data.TypedEncoding.Common.Types.Enc
import           Data.Proxy


-- |
-- Currently this is the recommended way of recreating encoding from trusted input,
-- if avoiding cost of "Data.TypedEncoding.Common.Types.Validation" is important.
--    
-- @since 0.1.0.0 
unsafeSetPayload :: conf -> str -> Enc enc conf str 
unsafeSetPayload  = UnsafeMkEnc Proxy 

-- |
-- @since 0.1.0.0
withUnsafeCoerce ::  (s1 -> s2) -> Enc e1 c s1 -> Enc e2 c s2
withUnsafeCoerce f (UnsafeMkEnc _ conf str)  = UnsafeMkEnc Proxy conf (f str)

-- |
-- @since 0.3.0.0
withUnsafeCoerceF :: forall e1 e2 f c s1 s2 . Functor f => (s1 -> f s2) -> Enc e1 c s1 -> f (Enc e2 c s2)
withUnsafeCoerceF f (UnsafeMkEnc _ conf str)  = UnsafeMkEnc Proxy conf <$> f str

-- |
-- @since 0.1.0.0 
unsafeChangePayload ::  (s1 -> s2) -> Enc e c s1 -> Enc e c s2
unsafeChangePayload f (UnsafeMkEnc p conf str)  = UnsafeMkEnc p conf (f str) 
