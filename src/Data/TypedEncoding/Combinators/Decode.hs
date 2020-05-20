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

import           Data.TypedEncoding.Common.Types (UnexpectedDecodeEx(..))
import           Data.TypedEncoding.Internal.Enc
import           Data.TypedEncoding.Common.Types.Decoding
import           Data.TypedEncoding.Combinators.Common

import           Data.TypedEncoding.Common.Class.Util
import           Data.TypedEncoding.Common.Class.Decode
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits


-- * Convenience combinators which mimic pre-v0.3 type signatures. These assume that @algs@ are the same as @nms@

decF :: forall nm xs f c str . (Decode f nm nm c str) => Enc (nm ': xs) c str -> f (Enc xs c str)
decF = decF' @nm @nm

decFAll :: forall nms f c str . (Monad f,  DecodeAll f nms nms c str) => 
               Enc nms c str
               -> f (Enc ('[]::[Symbol]) c str)  
decFAll = decFAll' @nms @nms 

-- | 
-- 
decAll :: forall nms c str . (DecodeAll Identity nms nms c str) =>
               Enc nms c str 
               -> Enc ('[]::[Symbol]) c str 
decAll = decAll' @nms @nms  

decFPart :: forall xs xsf f c str . (Monad f, DecodeAll f xs xs c str) => Enc (Append xs xsf) c str -> f (Enc xsf c str)
decFPart = decFPart' @xs @xs 

decPart :: forall xs xsf c str . (DecodeAll Identity xs xs c str) => Enc (Append xs xsf) c str -> Enc xsf c str   
decPart = decPart' @xs @xs  



-- * Convenience combinators which mimic pre-v0.3 type signatures. These do not try to figure out @algs@ or assume much about them

decF' :: forall alg nm xs f c str . (Decode f nm alg c str) => Enc (nm ': xs) c str -> f (Enc xs c str)
decF' = runDecoding (decoding @f @nm @alg)

decFAll' :: forall algs nms f c str . (Monad f,  DecodeAll f nms algs c str) => 
               Enc nms c str
               -> f (Enc ('[]::[Symbol]) c str)  
decFAll' = runDecodings @algs @nms @f decodings 

-- | 
-- 
decAll' :: forall algs nms c str . (DecodeAll Identity nms algs c str) =>
               Enc nms c str 
               -> Enc ('[]::[Symbol]) c str 
decAll' = runIdentity . decFAll' @algs 

decFPart' :: forall algs xs xsf f c str . (Monad f, DecodeAll f xs algs c str) => Enc (Append xs xsf) c str -> f (Enc xsf c str)
decFPart' = aboveF @xsf @xs @'[] (decFAll' @algs)  
-- decFPart' (MkEnc _ conf str) =   
--     let re :: f (Enc '[] c str) = decFAll' @algs @xs $ MkEnc Proxy conf str
--     in  MkEnc Proxy conf . getPayload <$> re

decPart' :: forall algs xs xsf c str . (DecodeAll Identity xs algs c str) => Enc (Append xs xsf) c str -> Enc xsf c str   
decPart' = runIdentity . decFPart' @algs @xs


