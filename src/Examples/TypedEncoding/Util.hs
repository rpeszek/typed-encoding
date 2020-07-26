
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Some helper definitions used in /Examples/
module Examples.TypedEncoding.Util where


-- | Polymorphic data payloads used to encode/decode
--
-- This class is intended for example use only and will be moved to Example modules.
-- 
-- Use your favorite polymorphic records / ad-hock product polymorphism library.
--
-- @since 0.1.0.0
class HasA a c where
    has :: c -> a

instance HasA () c where
    has = const ()
