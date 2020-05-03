
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Data.TypedEncoding.Internal.Util where

import           Data.Proxy
import           GHC.TypeLits

explainBool :: (a -> err) -> (a, Bool) -> Either err a
explainBool _ (a, True) = Right a
explainBool f (a, False) = Left $ f a 


-- | Sometimes is easier to pass around a proxy than do TypeApplications
proxiedId :: Proxy a -> a -> a
proxiedId _ = id

-- | explicit mapM
extractEither :: Traversable t => t (Either err a) -> Either err (t a)
extractEither = sequence -- mapM id

withSomeSymbol :: SomeSymbol -> (forall x. KnownSymbol x => Proxy x -> r) -> r
withSomeSymbol s fn = case s of 
    SomeSymbol p -> fn p