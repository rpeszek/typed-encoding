
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Encoding.Internal.Class where

import          Data.Encoding.Internal.Types (Enc(..), toEncoding)
import          Data.Proxy

class Encode instr outstr where 
    encode :: instr -> outstr

class EncodeAll (xs :: [k]) c str where
    encodeAll :: (Enc '[] c str) -> (Enc xs c str)

instance EncodeAll '[] c str where
    encodeAll (MkEnc _ c str) = toEncoding c str 

instance (EncodeAll xs c str, Encode (Enc xs c str) (Enc (x ': xs) c str)) => EncodeAll (x ': xs) c str where
    encodeAll str = 
        let r :: Enc xs c str = encodeAll str
        in encode r

class Decode instr outstr where    
    decode :: instr -> Either String outstr

class DecodeAll (xs :: [k]) c str where
    decodeAll :: (Enc xs c str) ->  Either String (Enc '[] c str)

instance DecodeAll '[] c str where
    decodeAll (MkEnc _ c str) = Right $ toEncoding c str 

instance (DecodeAll xs c str, Decode (Enc (x ': xs) c str) (Enc (xs) c str)) => DecodeAll (x ': xs) c str where
    decodeAll str = 
        let re :: Either String (Enc xs c str) = decode str
        in case re of 
            Left err -> Left err
            Right r -> decodeAll r

class DecodeLenient instr outstr where    
    decodeLenient :: instr -> outstr

class DecodeAllLenient (xs :: [k]) c str where
    decodeAllLenient :: (Enc xs c str) -> (Enc '[] c str)

instance DecodeAllLenient '[] c str where
    decodeAllLenient (MkEnc _ c str) = toEncoding c str

instance (DecodeAllLenient xs c str, DecodeLenient (Enc (x ': xs) c str) (Enc (xs) c str)) => DecodeAllLenient (x ': xs) c str where
    decodeAllLenient str = 
        let r :: Enc xs c str = decodeLenient str
        in decodeAllLenient r

class HasA c a where
    has :: Proxy a -> c -> a

instance HasA a () where
    has _ = const ()