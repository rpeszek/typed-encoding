{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Encoding.Internal.Types where

import           Data.Proxy
import           Data.Functor.Identity

-- Not a Functor on purpose
data Enc enc conf str where
    -- | constructor is to be treated as Impl to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy enc -> conf -> str -> Enc enc conf str
    deriving (Show) 

toEncoding :: conf -> str -> Enc '[] conf str
toEncoding conf str = MkEnc Proxy conf str

fromEncoding :: Enc '[] conf str -> str
fromEncoding = unsafeGetPayload

unsafeGetPayload :: Enc enc conf str -> str  
unsafeGetPayload (MkEnc _ _ str) = str

implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (\c -> f)

implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f conf str


implTranP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP f  = implTranF' (\c -> pure . f)

implTranP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP' f  = implTranF' (\c -> pure . f c)
 

-- | Functor instance are to be used by Encode and Decode instance implemenations only
newtype Impl enc conf str = Impl {unImpl :: Enc enc conf str} deriving (Show)

withImpl :: (Impl e c s1 -> Impl e c s2) -> Enc e c s1 -> Enc e c s2
withImpl f enc = unImpl . f $ Impl enc

instance Functor (Impl enc conf) where
    fmap f (Impl (MkEnc p c x)) = Impl (MkEnc p c (f x))

-- we could add these if needed
--    
-- instance Default conf => Applicative (Impl enc conf) where
--     pure = Priv . MkEnc Proxy def  
--     Priv (MkEnc p c1 f) <*> Priv (MkEnc _ c2 x) = Priv (MkEnc p (mappend c1 c2) (f x))  

-- instance Default conf => Monad (Impl enc conf) where 
--     Priv (MkEnc _ _ x) >>= f = f x     

--TODO JSON instances    


-- class HasA c a | c -> a where
--     has :: c -> a

-- data Named (nm :: k) a where
--     MkNamed :: Proxy nm -> a -> Named nm a

-- tst :: Enc (Named "Test" Int) String
-- tst = MkEnc (Proxy :: Proxy (Named "Test" Int)) ""