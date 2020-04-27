module Data.TypedEncoding.Internal.Util where

import           Data.Proxy

explainBool :: (a -> err) -> (a, Bool) -> Either err a
explainBool _ (a, True) = Right a
explainBool f (a, False) = Left $ f a 


-- | Sometimes is actually less code and better code reuse without TypeApplications
proxiedId :: Proxy a -> a -> a
proxiedId _ = id


