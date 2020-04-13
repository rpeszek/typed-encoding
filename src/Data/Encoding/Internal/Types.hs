{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Encoding.Internal.Types where

import           Data.Proxy

-- Not a Functor on purpose
data Enc enc conf str where
    -- | constructor is to be treated as private to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy enc -> conf -> str -> Enc enc conf str
    deriving (Functor, Show) -- TODO remove Functor from here

toEncoding :: conf -> str -> Enc '[] conf str
toEncoding conf str = MkEnc Proxy conf str

fromEncoding :: Enc '[] conf str -> str
fromEncoding = unsafeGetPayload

unsafeGetPayload :: Enc enc conf str -> str  
unsafeGetPayload (MkEnc _ _ str) = str



implTran :: (str -> str) -> Enc enc1 conf str -> Enc enc2 conf str
implTran f (MkEnc _ conf str) = MkEnc Proxy conf (f str)

implTranErr :: (str -> Either a str) -> Enc enc1 conf str -> Either a (Enc enc2 conf str)
implTranErr f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f str

implTran' :: (conf -> str -> str) -> Enc enc1 conf str -> Enc enc2 conf str
implTran' f (MkEnc _ conf str) = MkEnc Proxy conf (f conf str)

implTranErr' :: (conf -> str -> Either a str) -> Enc enc1 conf str -> Either a (Enc enc2 conf str)
implTranErr' f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f conf str

-- | Applicative and Monad instance are to be used by Encode and Decode instance implemenations only
newtype Private enc conf str = Priv {unPriv :: Enc enc conf str} deriving (Functor, Show)

-- instance Default conf => Applicative (Private enc conf) where
--     pure = Priv . MkEnc Proxy def  
--     Priv (MkEnc p c1 f) <*> Priv (MkEnc _ c2 x) = Priv (MkEnc p (mappend c1 c2) (f x))  

-- instance Default conf => Monad (Private enc conf) where 
--     Priv (MkEnc _ _ x) >>= f = f x     

--TODO JSON instances    


-- class HasA c a | c -> a where
--     has :: c -> a

-- data Named (nm :: k) a where
--     MkNamed :: Proxy nm -> a -> Named nm a

-- tst :: Enc (Named "Test" Int) String
-- tst = MkEnc (Proxy :: Proxy (Named "Test" Int)) ""