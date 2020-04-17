
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Encoding.Internal.Class where

import          Data.Encoding.Internal.Types (Enc(..), toEncoding)
import          Data.Proxy
import          Data.Functor.Identity


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

-- | This appears to need INCOHERENT instances 
-- and is kept separate from DecodeFAll
class DecodeFPart f (xsf :: [k]) (xst :: [k]) c str where
    decodeFPart :: Proxy xst -> (Enc xsf c str) ->  f (Enc xst c str)

-- | for reason I do not understand this does not work:
--
-- instance {-# INCOHERENT #-} (Applicative f, xs1 ~ xs2) => DecodeFPart f xs1 xs2 c str where
--     decodeFPart p (MkEnc _ c str) = pure $ (MkEnc p c str)
--
-- but this does
instance {-# INCOHERENT #-} (Applicative f) => DecodeFPart f xs1 xs1 c str where
    decodeFPart p (MkEnc _ c str) = pure $ (MkEnc p c str)

instance {-# INCOHERENT #-} (Monad f, DecodeFPart f xs xs0 c str, DecodeF f (Enc (x ': xs) c str) (Enc (xs) c str)) => DecodeFPart f (x ': xs) xs0 c str where
    decodeFPart p str = 
        let re :: f (Enc xs c str) = decodeF str
        in re >>= decodeFPart p

decodePart :: DecodeFPart Identity (xsf :: [k]) (xst :: [k]) c str => 
              Proxy xst
              -> (Enc xsf c str)
              -> (Enc xst c str) 
decodePart p = runIdentity . decodeFPart p 

-- | This appears to need INCOHERENT instances 
-- and is kept separate from EncodeFAll
class EncodeFPart f (xsf :: [k]) (xst :: [k]) c str where
    encodeFPart :: Proxy xsf -> (Enc xsf c str) ->  f (Enc xst c str)

-- | for reason I do not understand this does not work:
--
-- instance {-# INCOHERENT #-} (Applicative f, xs1 ~ xs2) => EncodeFPart f xs1 xs2 c str where
--     encodeFPart p (MkEnc _ c str) = pure $ (MkEnc p c str)
--
-- but this does
instance {-# INCOHERENT #-} (Applicative f) => EncodeFPart f xs1 xs1 c str where
    encodeFPart p (MkEnc _ c str) = pure $ (MkEnc p c str)

instance {-# INCOHERENT #-} (Monad f, EncodeFPart f xs0 xs c str, EncodeF f (Enc (xs) c str) (Enc (x ': xs) c str)) => EncodeFPart f xs0 (x ': xs) c str where
    encodeFPart p str = 
        let re :: f (Enc xs c str) = encodeFPart p str
        in re >>= encodeF

encodePart :: EncodeFPart Identity (xsf :: [k]) (xst :: [k]) c str => 
              Proxy xsf
              -> (Enc xsf c str)
              -> (Enc xst c str) 
encodePart p = runIdentity . encodeFPart p 

-- 

-- Other classes --

-- | Converts keeping encoding annotations
class Convert (xs :: [k]) str1 str2 where
    convert :: Enc (xs :: [k]) c str1 ->  Enc xs c str2

-- | Polymorphic data payloads used to encode/decode
class HasA c a where
    has :: Proxy a -> c -> a

instance HasA a () where
    has _ = const ()