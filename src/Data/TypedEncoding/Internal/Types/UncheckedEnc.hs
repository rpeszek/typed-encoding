{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Internal definition of types

module Data.TypedEncoding.Internal.Types.UncheckedEnc where

import           Data.Proxy
import           Data.TypedEncoding.Internal.Class.Util
import           Data.TypedEncoding.Internal.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T


-- * UncheckedEnc for recreate, similar to CheckedEnc only not verified

-- | Represents some encoded string where encoding was not validated.
--
-- Similar to 'Data.TypedEncoding.Internal.Types.CheckedEnc' but unlike
-- @CheckedEnc@ it can contain payloads that have invalid encoding.
-- 
data UncheckedEnc c str = MkUncheckedEnc [EncAnn] c str deriving (Show, Eq)

toUncheckedEnc :: [EncAnn] -> c -> str -> UncheckedEnc c str
toUncheckedEnc = MkUncheckedEnc

getUncheckedEncAnn :: UncheckedEnc c str -> [EncAnn]
getUncheckedEncAnn (MkUncheckedEnc ann _ _) = ann

verifyAnn :: forall xs c str . SymbolList xs => UncheckedEnc c str -> Either String (UncheckedEnc c str)
verifyAnn x@(MkUncheckedEnc xs _ _) = 
    let p = Proxy :: Proxy xs
    in if symbolVals @ xs == xs
       then Right x
       else Left $ "UncheckedEnc has not matching annotation " ++ displ xs

-- |
-- >>> displ $ MkUncheckedEnc ["TEST"] () ("hello" :: T.Text)
-- "MkUncheckedEnc [TEST] () (Text hello)"
instance (Show c, Displ str) => Displ (UncheckedEnc c str) where
    displ (MkUncheckedEnc xs c s) = 
        "MkUncheckedEnc " ++ displ xs ++ " " ++ show c ++ " " ++ displ s
