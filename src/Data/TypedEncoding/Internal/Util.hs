module Data.TypedEncoding.Internal.Util where

import           Data.Proxy

explainBool :: (a -> err) -> (a, Bool) -> Either err a
explainBool _ (a, True) = Right a
explainBool f (a, False) = Left $ f a 


-- | Sometimes is easier to pass around a proxy than do TypeApplications
proxiedId :: Proxy a -> a -> a
proxiedId _ = id

-- | explicit mapM
extractEither :: Traversable t => t (Either err a) -> Either err (t a)
extractEither tx = mapM id tx