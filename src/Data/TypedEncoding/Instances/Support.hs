
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | Exports for instance creation.
-- 
-- Contains typical things needed when implementing
-- encoding, decoding, recreate, or type to string conversions.
module Data.TypedEncoding.Instances.Support (
    module Data.TypedEncoding.Instances.Support
    -- * Types
    , module Data.TypedEncoding.Common.Types
    -- * Classes
    , module Data.TypedEncoding.Common.Class
    -- * Combinators
    , module Data.TypedEncoding.Instances.Support.Helpers
    -- * Type level conveniences
    , module Data.TypedEncoding.Common.Util.TypeLits
    , module Data.TypedEncoding.Combinators.Unsafe
   ) where
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Common.Class 
import           Data.TypedEncoding.Instances.Support.Helpers 
import           Data.TypedEncoding.Common.Util.TypeLits
import           Data.TypedEncoding.Combinators.Unsafe 

import           GHC.TypeLits
import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XTypeApplications


-- * Decoding 

-- | Universal decoding for all "r-" types
decAnyR :: (IsR s ~ 'True, AlgNm s ~ x, Applicative f) => Decoding f s x c str
decAnyR  = mkDecoding $ implTranP id 

-- * Validation

validFromDec :: forall nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Decoding (Either UnexpectedDecodeEx) nm nm c str -> Validation f nm nm c str  
validFromDec = validFromDec' @nm @nm 

validFromDec' :: forall alg nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Decoding (Either UnexpectedDecodeEx) nm alg c str -> Validation f nm alg c str  
validFromDec' (UnsafeMkDecoding p fn) = UnsafeMkValidation p (decAsRecreateErr . fn)
   where 
        decAsRecreateErr :: Either UnexpectedDecodeEx a -> f a
        decAsRecreateErr (Left (UnexpectedDecodeEx p err)) = recoveryErr $ RecreateEx p err
        decAsRecreateErr (Right r) = pure r


validR :: forall nm f c str . (IsR nm ~ 'True, KnownSymbol nm, RecreateErr f, Applicative f) => Encoding (Either EncodeEx) nm nm c str -> Validation f nm nm c str  
validR = validFromEnc' @nm @nm 

-- | Can cause slow compilation if used
validR' :: forall nm f c str alg . (IsR nm ~ 'True, AlgNm nm ~ alg, KnownSymbol nm, RecreateErr f, Applicative f) =>  Encoding (Either EncodeEx) nm alg c str -> Validation f nm alg c str  
validR' = validFromEnc' @alg @nm 


validFromEnc' :: forall alg nm f c str . (KnownSymbol nm, RecreateErr f, Applicative f) => Encoding (Either EncodeEx) nm alg c str -> Validation f nm alg c str  
validFromEnc' (UnsafeMkEncoding p fn) = UnsafeMkValidation p (encAsRecreateErr . rfn) 
   where
        encAsRecreateErr :: Either EncodeEx a -> f a
        encAsRecreateErr (Left (EncodeEx p err)) = recoveryErr $ RecreateEx p err
        encAsRecreateErr (Right r) = pure r 
        rfn :: forall (xs :: [Symbol]) . Enc (nm ': xs) c str -> Either EncodeEx (Enc xs c str)
        rfn (MkEnc _ conf str)  =    
            let re = fn $ MkEnc Proxy conf str
            in  MkEnc Proxy conf . getPayload <$> re 



-- TODO v0.3 remove
-- | Universal decode for all "r-" types
decFR :: (IsR s ~ 'True, Applicative f) => 
            Enc (s ': xs) c str -> f (Enc xs c str) 
decFR = implTranP id 

-- TODO v0.3 remove

-- | 
-- Manual recreate step combinator converting @"r-"@ encode function to a recreate step.
--
-- For "r-" encoding recreate and encode are the same other than the exception type used. 
--
-- The convention in @typed-encoding@ is to implement encode and convert it to recreate.
recWithEncR :: forall (s :: Symbol) xs c str . (IsR s ~ 'True) 
                       => (Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)) 
                       -> Enc xs c str -> Either RecreateEx (Enc (s ': xs) c str)
recWithEncR = unsafeRecWithEncR


unsafeRecWithEncR :: forall (s :: Symbol) xs c str .
                       (Enc xs c str -> Either EncodeEx (Enc (s ': xs) c str)) 
                       -> Enc xs c str -> Either RecreateEx (Enc (s ': xs) c str)
unsafeRecWithEncR fn = either (Left . encToRecrEx) Right . fn

-- |
-- >>> :kind! IsR "r-UPPER"
-- ...
-- ... 'True
--
-- >>> :kind! IsR "do-UPPER"
-- ...
-- = (TypeError ... 
type family IsR (s :: Symbol) :: Bool where
    IsR s = AcceptEq ('Text "Not restriction encoding " ':<>: ShowType s ) (CmpSymbol "r-" (Take 2 s))


type family IsROrEmpty (s :: Symbol) :: Bool where
    IsROrEmpty "" = True
    IsROrEmpty x  = IsR x
