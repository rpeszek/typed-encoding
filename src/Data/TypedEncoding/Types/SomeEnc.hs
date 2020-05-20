{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module defines 'SomeEnc' - existentially quantified version of @Enc@
-- and basic combinators

module Data.TypedEncoding.Types.SomeEnc where

import           Data.TypedEncoding.Internal.Enc
import           Data.TypedEncoding.Class.Util
import           Data.TypedEncoding.Types.SomeAnnotation
import           Data.TypedEncoding.Types.CheckedEnc

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T



-- | Existentially quantified quanitified @Enc@
-- effectively isomorphic to 'CheckedEnc'
data SomeEnc conf str where
    MkSomeEnc :: SymbolList xs => Enc xs conf str -> SomeEnc conf str
    
withSomeEnc :: SomeEnc conf str -> (forall xs . SymbolList xs => Enc xs conf str -> r) -> r
withSomeEnc (MkSomeEnc enc) f = f enc

toSome :: SymbolList xs => Enc xs conf str -> SomeEnc conf str
toSome = MkSomeEnc

-- | 
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> someToChecked . MkSomeEnc $ enctest
-- MkCheckedEnc ["TEST"] () "hello"
someToChecked :: SomeEnc conf str -> CheckedEnc conf str
someToChecked se = withSomeEnc se toCheckedEnc

-- | 
-- >>> let tst = unsafeCheckedEnc ["TEST"] () "test"
-- >>> displ $ checkedToSome tst
-- "Some (MkEnc '[TEST] () (String test))"
checkedToSome :: CheckedEnc conf str -> SomeEnc conf str
checkedToSome (MkCheckedEnc xs c s) = withSomeAnnotation (someAnnValue xs) (\p -> MkSomeEnc (MkEnc p c s))


-- |
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ $ MkSomeEnc enctest
-- "Some (MkEnc '[TEST] () (Text hello))"
instance (Show c, Displ str) => Displ (SomeEnc c str) where
    displ (MkSomeEnc en) = 
       "Some (" ++ displ en ++ ")"
