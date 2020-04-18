module Data.Encoding.Internal.Utils where


explainBool :: (a -> err) -> (a, Bool) -> Either err a
explainBool _ (a, True) = Right a
explainBool f (a, False) = Left $ f a 