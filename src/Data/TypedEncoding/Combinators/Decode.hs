{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Combinators reexported in Data.TypedEncoding
module Data.TypedEncoding.Combinators.Decode where

import           Data.TypedEncoding.Common.Types.Enc
import           Data.TypedEncoding.Common.Types.Decoding
import           Data.TypedEncoding.Combinators.Common

import           Data.TypedEncoding.Common.Class.Util
import           Data.TypedEncoding.Common.Class.Decode
import           Data.Functor.Identity
import           GHC.TypeLits


-- * Convenience combinators which mimic pre-v0.3 type signatures. These assume that @algs@ are the same as @nms@

decodeF :: forall nm xs f c str . (Decode f nm nm c str) => Enc (nm ': xs) c str -> f (Enc xs c str)
decodeF = decodeF' @nm @nm

decodeFAll :: forall nms f c str . (Monad f,  DecodeAll f nms nms c str) => 
               Enc nms c str
               -> f (Enc ('[]::[Symbol]) c str)  
decodeFAll = decodeFAll' @nms @nms 

-- | 
-- 
decodeAll :: forall nms c str . (DecodeAll Identity nms nms c str) =>
               Enc nms c str 
               -> Enc ('[]::[Symbol]) c str 
decodeAll = decodeAll' @nms @nms  

decodeFPart :: forall xs xsf f c str . (Monad f, DecodeAll f xs xs c str) => Enc (Append xs xsf) c str -> f (Enc xsf c str)
decodeFPart = decodeFPart' @xs @xs 

decodePart :: forall xs xsf c str . (DecodeAll Identity xs xs c str) => Enc (Append xs xsf) c str -> Enc xsf c str   
decodePart = decodePart' @xs @xs  



-- * Convenience combinators which mimic pre-v0.3 type signatures. These do not try to figure out @algs@ or assume much about them

decodeF' :: forall alg nm xs f c str . (Decode f nm alg c str) => Enc (nm ': xs) c str -> f (Enc xs c str)
decodeF' = runDecoding (decoding @f @nm @alg)

decodeFAll' :: forall algs nms f c str . (Monad f,  DecodeAll f nms algs c str) => 
               Enc nms c str
               -> f (Enc ('[]::[Symbol]) c str)  
decodeFAll' = runDecodings @algs @nms @f decodings 

-- | 
-- 
decodeAll' :: forall algs nms c str . (DecodeAll Identity nms algs c str) =>
               Enc nms c str 
               -> Enc ('[]::[Symbol]) c str 
decodeAll' = runIdentity . decodeFAll' @algs 

decodeFPart' :: forall algs xs xsf f c str . (Monad f, DecodeAll f xs algs c str) => Enc (Append xs xsf) c str -> f (Enc xsf c str)
decodeFPart' = aboveF @xsf @xs @'[] (decodeFAll' @algs)  
-- decodeFPart' (UnsafeMkEnc _ conf str) =   
--     let re :: f (Enc '[] c str) = decodeFAll' @algs @xs $ UnsafeMkEnc Proxy conf str
--     in  UnsafeMkEnc Proxy conf . getPayload <$> re

decodePart' :: forall algs xs xsf c str . (DecodeAll Identity xs algs c str) => Enc (Append xs xsf) c str -> Enc xsf c str   
decodePart' = runIdentity . decodeFPart' @algs @xs


