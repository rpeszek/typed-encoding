
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

module Data.Encoding.Internal.Class where

import Data.Encoding.Internal.Types (Enc)

class Encode (enc :: k) str where 
    encode :: str -> Enc enc str

class Decode (enc :: k) str where    
    decode :: Enc enc str -> Either String str

class DecodeLenient enc str where    
    decodeLenient :: Enc enc str -> str
