
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.TypedEncoding.Internal.Combinators where

import           Data.String
import           Data.Proxy
import           Data.TypedEncoding.Internal.Types


-- | allows to fold payload in Enc to create another Enc
foldEnc :: (Foldable f, Functor f) => c -> (s1 -> s2 -> s2) -> s2 -> f (Enc (xs1 :: [k]) c s1) -> Enc (xs2 :: [k]) c s2
foldEnc c f sinit ts = unsafeSetPayload c . foldr f sinit . fmap getPayload $ ts


foldEnc' :: (Foldable f, Functor f, IsString s2) => c -> (s1 -> s2 -> s2) -> f (Enc (xs1 :: [k]) c s1) -> Enc (xs2 :: [k]) c s2
foldEnc' c f = foldEnc c f ""

--    toEncString x = unsafeSetPayload () . foldr (\a b-> if b == "" then a else a <> "." <> b) "" . fmap getPayload $ x
