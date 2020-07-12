{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module defines 'SomeEnc' - existentially quantified version of @Enc@
-- and basic combinators.
--
-- This construction is common to more dependently typed Haskell but is 
-- isomorphic to 'Data.TypedEncoding.Common.Types.CheckedEnc.CheckedEnc'.  
--
-- Post v0.4 /typed-encoding/ supports @CheckedEnc@ while @SomeEnc@ remains as
-- as example.
--
-- (Moved from @Data.TypedEncoding.Common.Types.SomeEnc@ in previous versions)
--
-- @since 0.5.0.0

module Examples.TypedEncoding.SomeEnc where

import           Data.TypedEncoding.Common.Types.Enc
import           Data.TypedEncoding.Common.Class.Util
import           Data.TypedEncoding.Common.Types.CheckedEnc
import           Examples.TypedEncoding.SomeEnc.SomeAnnotation

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T
-- >>> import Data.TypedEncoding.Combinators.Unsafe



-- | Existentially quantified quantified @Enc@
-- effectively isomorphic to 'CheckedEnc'
--
-- @since 0.2.0.0
data SomeEnc conf str where
    MkSomeEnc :: SymbolList xs => Enc xs conf str -> SomeEnc conf str

-- |
-- @since 0.2.0.0   
withSomeEnc :: SomeEnc conf str -> (forall xs . SymbolList xs => Enc xs conf str -> r) -> r
withSomeEnc (MkSomeEnc enc) f = f enc

-- |
-- @since 0.2.0.0 
toSome :: SymbolList xs => Enc xs conf str -> SomeEnc conf str
toSome = MkSomeEnc

-- | 
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> someToChecked . MkSomeEnc $ enctest
-- UnsafeMkCheckedEnc ["TEST"] () "hello"
-- 
-- @since 0.2.0.0 
someToChecked :: SomeEnc conf str -> CheckedEnc conf str
someToChecked se = withSomeEnc se toCheckedEnc

-- | 
-- >>> let tst = unsafeCheckedEnc ["TEST"] () "test"
-- >>> displ $ checkedToSome tst
-- "Some (Enc '[TEST] () (String test))"
-- 
-- @since 0.2.0.0 s
checkedToSome :: CheckedEnc conf str -> SomeEnc conf str
checkedToSome (UnsafeMkCheckedEnc xs c s) = withSomeAnnotation (someAnnValue xs) (\p -> MkSomeEnc (UnsafeMkEnc p c s))


-- |
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ $ MkSomeEnc enctest
-- "Some (Enc '[TEST] () (Text hello))"
instance (Show c, Displ str) => Displ (SomeEnc c str) where
    displ (MkSomeEnc en) = 
       "Some (" ++ displ en ++ ")"
