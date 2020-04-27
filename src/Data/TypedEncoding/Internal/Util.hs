module Data.TypedEncoding.Internal.Util where

import           Data.Proxy

explainBool :: (a -> err) -> (a, Bool) -> Either err a
explainBool _ (a, True) = Right a
explainBool f (a, False) = Left $ f a 



proxiedId :: Proxy a -> a -> a
proxiedId _ = id


