{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Encoding.Internal.Types where

import           Data.Proxy

data Enc enc str where
    -- | constructor is to be treated as private to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy enc -> str -> Enc enc str
    deriving (Functor, Show) 

toEncoding :: str -> Enc '[] str
toEncoding str = MkEnc Proxy str

fromEncoding :: Enc '[] str -> str
fromEncoding = getPayload

unsafeGetPayload :: Enc enc str -> str  
unsafeGetPayload = getPayload

-- private, to be used by Encode/decode instances only
getPayload :: Enc enc str -> str 
getPayload (MkEnc _ str) = str

-- | Applicative and Monad instance are to be used by Encode and Decode instance implemenations only
newtype Private enc str = Priv {unPriv :: Enc enc str} deriving (Functor, Show)

instance Applicative (Private enc) where
    pure = Priv . MkEnc (Proxy)  
    Priv (MkEnc p f) <*> Priv (MkEnc _ x) = Priv (MkEnc p (f x))  

instance Monad (Private enc) where 
    Priv (MkEnc p x) >>= f = f x     

--TODO JSON instances    