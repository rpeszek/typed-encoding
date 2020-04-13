
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Encoding.Internal.Class where

import          Data.Encoding.Internal.Types (Enc(..), toEncoding)
import          Data.Proxy

class Encode instr outstr where 
    encode :: instr -> outstr

class EncodeAll (xs :: [k]) str where
    encodeAll :: (Enc '[] str) -> (Enc xs str)

instance EncodeAll '[] str where
    encodeAll (MkEnc _ str) = toEncoding str -- MkEnc Proxy str

instance (EncodeAll xs str, Encode (Enc xs str) (Enc (x ': xs) str)) => EncodeAll (x ': xs) str where
    encodeAll str = 
        let r :: Enc xs str = encodeAll str
        in encode r

class Decode instr outstr where    
    decode :: instr -> Either String outstr

class DecodeAll (xs :: [k]) str where
    decodeAll :: (Enc xs str) ->  Either String (Enc '[] str)

instance DecodeAll '[] str where
    decodeAll (MkEnc _ str) = Right $ toEncoding str -- MkEnc Proxy str

instance (DecodeAll xs str, Decode (Enc (x ': xs) str) (Enc (xs) str)) => DecodeAll (x ': xs) str where
    decodeAll str = 
        let re :: Either String (Enc xs str) = decode str
        in case re of 
            Left err -> Left err
            Right r -> decodeAll r

class DecodeLenient instr outstr where    
    decodeLenient :: instr -> outstr

class DecodeAllLenient (xs :: [k]) str where
    decodeAllLenient :: (Enc xs str) -> (Enc '[] str)

instance DecodeAllLenient '[] str where
    decodeAllLenient (MkEnc _ str) = toEncoding str -- MkEnc Proxy str

instance (DecodeAllLenient xs str, DecodeLenient (Enc (x ': xs) str) (Enc (xs) str)) => DecodeAllLenient (x ': xs) str where
    decodeAllLenient str = 
        let r :: Enc xs str = decodeLenient str
        in decodeAllLenient r
