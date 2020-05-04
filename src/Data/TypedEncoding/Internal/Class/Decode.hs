
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Internal.Class.Decode where

import           Data.TypedEncoding.Internal.Class.Util

import           Data.TypedEncoding.Internal.Types (Enc(..) 
                                              , toEncoding
                                              , getPayload
                                              , UnexpectedDecodeEx(..))
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits


class DecodeF f instr outstr where    
    decodeF :: instr -> f outstr

class DecodeFAll f (xs :: [Symbol]) c str where
    decodeFAll :: Enc xs c str ->  f (Enc '[] c str)

instance Applicative f => DecodeFAll f '[] c str where
    decodeFAll (MkEnc _ c str) = pure $ toEncoding c str 

instance (Monad f, DecodeFAll f xs c str, DecodeF f (Enc (x ': xs) c str) (Enc xs c str)) => DecodeFAll f (x ': xs) c str where
    decodeFAll str = 
        let re :: f (Enc xs c str) = decodeF str
        in re >>= decodeFAll

decodeAll :: forall xs c str . DecodeFAll Identity (xs :: [Symbol]) c str => 
              Enc xs c str
              -> Enc '[] c str
decodeAll = runIdentity . decodeFAll 


decodeFPart_ :: forall f xs xsf c str . (Functor f, DecodeFAll f xs c str) => Proxy xs -> Enc (Append xs xsf) c str -> f (Enc xsf c str)
decodeFPart_ p (MkEnc _ conf str) = 
    let re :: f (Enc '[] c str) = decodeFAll $ MkEnc (Proxy :: Proxy xs) conf str
    in  MkEnc Proxy conf . getPayload <$> re 

decodeFPart :: forall (xs :: [Symbol]) xsf f c str . (Functor f, DecodeFAll f xs c str) =>  Enc (Append xs xsf) c str -> f (Enc xsf c str)
decodeFPart = decodeFPart_ (Proxy :: Proxy xs) 

decodePart_ :: DecodeFAll Identity (xs :: [Symbol]) c str => 
              Proxy xs 
              -> Enc (Append xs xsf) c str 
              -> Enc xsf c str 
decodePart_ p = runIdentity . decodeFPart_ p

decodePart :: forall (xs :: [Symbol]) xsf c str .  DecodeFAll Identity xs c str => 
              Enc (Append xs xsf) c str 
              -> Enc xsf c str
decodePart = decodePart_ (Proxy :: Proxy xs)              

-- | With type safety in place decoding errors should be unexpected.
-- This class can be used to provide extra info if decoding could fail
class UnexpectedDecodeErr f where 
    unexpectedDecodeErr :: UnexpectedDecodeEx -> f a

instance UnexpectedDecodeErr Identity where
    unexpectedDecodeErr x = fail $ show x

instance UnexpectedDecodeErr (Either UnexpectedDecodeEx) where
    unexpectedDecodeErr = Left 

asUnexpected_ :: (KnownSymbol x, UnexpectedDecodeErr f, Applicative f, Show err) => Proxy x -> Either err a -> f a
asUnexpected_ p (Left err) = unexpectedDecodeErr $ UnexpectedDecodeEx p err
asUnexpected_ _ (Right r) = pure r

asUnexpected :: forall x f err a . (KnownSymbol x, UnexpectedDecodeErr f, Applicative f, Show err) => Either err a -> f a
asUnexpected = asUnexpected_ (Proxy :: Proxy x)
