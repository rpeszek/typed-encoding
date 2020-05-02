{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE RankNTypes #-}

-- |
-- Internal definition of types

module Data.TypedEncoding.Internal.Types.Unchecked where

import           Data.Proxy
import           Data.TypedEncoding.Internal.Class.Util
import           Data.TypedEncoding.Internal.Types.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T


-- * Unchecked for recreate, similar to SomeEnc only not verified

-- | Represents some encoded string where encoding was not validated.
--
-- Similar to 'Data.TypedEncoding.Internal.Types.SomeEnc' but unlike
-- @SomeEnc@ it can contain payloads that have invalid encoding.
-- 
data Unchecked c str = MkUnchecked [SomeAnn] c str deriving (Show, Eq)

toUnchecked :: [SomeAnn] -> c -> str -> Unchecked c str
toUnchecked = MkUnchecked

getUncheckedAnn :: Unchecked c str -> [SomeAnn]
getUncheckedAnn (MkUnchecked ann _ _) = ann

verifyAnn :: forall xs c str . KnownAnnotation xs => Unchecked c str -> Either String (Unchecked c str)
verifyAnn x@(MkUnchecked xs _ _) = 
    let p = Proxy :: Proxy xs
    in if knownAnn @ xs == xs
       then Right x
       else Left $ "Unchecked has not matching annotation " ++ displ xs

-- |
-- >>> displ $ MkUnchecked ["TEST"] () ("hello" :: T.Text)
-- "MkUnchecked [TEST] () (Text hello)"
instance (Show c, Displ str) => Displ (Unchecked c str) where
    displ (MkUnchecked xs c s) = 
        "MkUnchecked " ++ displ xs ++ " " ++ show c ++ " " ++ displ s
