
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Encoding.Internal.Class where

import          Data.Encoding.Internal.Types (Enc(..), toEncoding, unsafeGetPayload, withUnsafeCoerce)
import          Data.Proxy
import          Data.Functor.Identity
import          GHC.TypeLits


class EncodeF f instr outstr where    
    encodeF :: instr -> f outstr

class EncodeFAll f (xs :: [k]) c str where
    encodeFAll :: (Enc '[] c str) -> f (Enc xs c str)

instance Applicative f => EncodeFAll f '[] c str where
    encodeFAll (MkEnc _ c str) = pure $ toEncoding c str 

instance (Monad f, EncodeFAll f xs c str, EncodeF f (Enc xs c str) (Enc (x ': xs) c str)) => EncodeFAll f (x ': xs) c str where
    encodeFAll str = 
        let re :: f (Enc xs c str) = encodeFAll str
        in re >>= encodeF


encodeAll :: EncodeFAll Identity (xs :: [k]) c str => 
              (Enc '[] c str) 
              -> (Enc xs c str)
encodeAll = runIdentity . encodeFAll             



class DecodeF f instr outstr where    
    decodeF :: instr -> f outstr

class DecodeFAll f (xs :: [k]) c str where
    decodeFAll :: (Enc xs c str) ->  f (Enc '[] c str)

instance Applicative f => DecodeFAll f '[] c str where
    decodeFAll (MkEnc _ c str) = pure $ toEncoding c str 

instance (Monad f, DecodeFAll f xs c str, DecodeF f (Enc (x ': xs) c str) (Enc (xs) c str)) => DecodeFAll f (x ': xs) c str where
    decodeFAll str = 
        let re :: f (Enc xs c str) = decodeF str
        in re >>= decodeFAll

decodeAll :: DecodeFAll Identity (xs :: [k]) c str => 
              (Enc xs c str)
              -> (Enc '[] c str) 
decodeAll = runIdentity . decodeFAll 


-- | TODO use singletons def instead?
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] xs = xs
    Append (y ': ys) xs = y ': (Append ys xs)

-- | Unsafe implemenation guarted by safe type definition
encodeFPart :: forall f xs xsf c str . (Functor f, EncodeFAll f xs c str) => Proxy xs -> (Enc xsf c str) -> f (Enc (Append xs xsf) c str)
encodeFPart p (MkEnc _ conf str) = 
    let re :: f (Enc xs c str) = encodeFAll $ MkEnc Proxy conf str
    in  (MkEnc Proxy conf . unsafeGetPayload) <$> re 

encodePart :: EncodeFAll Identity (xs :: [k]) c str => 
              Proxy xs 
              -> (Enc xsf c str)
              -> (Enc (Append xs xsf) c str) 
encodePart p = runIdentity . encodeFPart p

-- | Unsafe implemenation guarted by safe type definition
decodeFPart :: forall f xs xsf c str . (Functor f, DecodeFAll f xs c str) => Proxy xs -> (Enc (Append xs xsf) c str) -> f (Enc xsf c str)
decodeFPart p (MkEnc _ conf str) = 
    let re :: f (Enc '[] c str) = decodeFAll $ MkEnc (Proxy :: Proxy xs) conf str
    in  (MkEnc Proxy conf . unsafeGetPayload) <$> re 
 
decodePart :: DecodeFAll Identity (xs :: [k]) c str => 
              Proxy xs 
              -> (Enc (Append xs xsf) c str) 
              -> (Enc xsf c str) 
decodePart p = runIdentity . decodeFPart p

-- Other classes --
class Subset (x :: k) (y :: k) where
    inject :: Enc (x ': xs) c str ->  Enc (y ': xs) c str
    inject = withUnsafeCoerce id


-- | Polymorphic data payloads used to encode/decode
class HasA c a where
    has :: Proxy a -> c -> a

instance HasA a () where
    has _ = const ()