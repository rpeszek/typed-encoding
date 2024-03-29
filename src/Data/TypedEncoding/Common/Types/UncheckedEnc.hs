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
-- Defines @UncheckedEnc@ representing not verified encoding and basic combinators for using it. 
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.

module Data.TypedEncoding.Common.Types.UncheckedEnc where

import           Data.Proxy
import           Data.TypedEncoding.Common.Class.Common
import           Data.TypedEncoding.Common.Types.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T


-- * UncheckedEnc for validation, similar to CheckedEnc but not verified

-- | Represents some encoded string where encoding was not validated.
--
-- Encoding is not tracked at the type level.
--
-- Similar to 'Data.TypedEncoding.Common.Types.CheckedEnc' but unlike
-- @CheckedEnc@ it can contain payloads that have invalid encoding.
-- 
-- See 'Data.TypedEncoding.Combinators.Validate.check'
-- 
-- @since 0.2.0.0  
data UncheckedEnc c str = MkUncheckedEnc [EncAnn] c str deriving (Show, Eq)

-- |
-- @since 0.2.0.0 
toUncheckedEnc :: [EncAnn] -> c -> str -> UncheckedEnc c str
toUncheckedEnc = MkUncheckedEnc

-- |
-- @since 0.2.0.0 
getUncheckedEncAnn :: UncheckedEnc c str -> [EncAnn]
getUncheckedEncAnn (MkUncheckedEnc ann _ _) = ann


getUncheckedPayload :: forall c str . UncheckedEnc c str -> str
getUncheckedPayload (MkUncheckedEnc _ _ str) = str

-- |
-- @since 0.2.0.0 
verifyAnn :: forall xs c str . SymbolList xs => UncheckedEnc c str -> Either String (UncheckedEnc c str)
verifyAnn x@(MkUncheckedEnc xs _ _) = 
    let p = Proxy :: Proxy xs
    in if symbolVals @xs == xs
       then Right x
       else Left $ "UncheckedEnc has not matching annotation " ++ displ xs

-- |
-- >>> displ $ MkUncheckedEnc ["TEST"] () ("hello" :: T.Text)
-- "MkUncheckedEnc [TEST] () (Text hello)"
instance (Show c, Displ str) => Displ (UncheckedEnc c str) where
    displ (MkUncheckedEnc xs c s) = 
        "MkUncheckedEnc " ++ displ xs ++ " " ++ show c ++ " " ++ displ s
