



module Data.TypedEncoding.Internal.Util where


explainBool :: (a -> err) -> (a, Bool) -> Either err a
explainBool _ (a, True) = Right a
explainBool f (a, False) = Left $ f a 


-- -- | explicit mapM
-- extractEither :: Traversable t => t (Either err a) -> Either err (t a)
-- extractEither = sequence -- mapM id

