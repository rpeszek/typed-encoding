{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module defines 'CheckedEnc' - untyped ADT version of 'Enc'
--
-- A more dependently typed approach would be to define SomeEnc GADT
-- that accepts some @Enc '[..]@ in its constructor.  The approach here
-- turns out to be isomorphic to @SomeEnc@ approach.  Both, however, yield
-- somewhat different programming.  
--
-- Post v0.4 /typed-encoding/ does not support @SomeEnc@ and it remains only as an /Example/.
-- 
-- See "Examples.TypedEncoding.SomeEnc".
--
-- This module is re-exported in "Data.TypedEncoding" and it is best not to import it directly.


module Data.TypedEncoding.Common.Types.CheckedEnc where

import           Data.TypedEncoding.Common.Types.Enc
import           Data.TypedEncoding.Common.Types.Common
import           Data.TypedEncoding.Common.Class.Common
import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T
-- >>> import  Data.TypedEncoding.Combinators.Unsafe (unsafeSetPayload)


-- * Untyped Enc


-- | Represents some validated encoded string. 
--
-- @CheckedEnc@ is untyped version of 'Data.TypedEncoding.Common.Types.Enc.Enc'. 
-- @CheckedEnc@ contains verified encoded data, encoding is visible
-- at the value level only.
--
-- @since 0.2.0.0 
data CheckedEnc conf str = UnsafeMkCheckedEnc [EncAnn] conf str -- ^ @since 0.3.0.0
                                                                -- Constructor renamed from previous versions
                                                                -- This constructor is considered unsafe as pattern matching on it and
                                                                -- using it allows access to the encoded payload.
     deriving (Show, Eq) 

-- |
-- @since 0.2.0.0
unsafeCheckedEnc:: [EncAnn] -> c -> s -> CheckedEnc c s
unsafeCheckedEnc = UnsafeMkCheckedEnc

-- |
-- @since 0.2.0.0
getCheckedPayload :: CheckedEnc conf str -> str
getCheckedPayload = snd . getCheckedEncPayload

-- |
-- @since 0.2.0.0
getCheckedEncPayload :: CheckedEnc conf str -> ([EncAnn], str) 
getCheckedEncPayload (UnsafeMkCheckedEnc t _ s) = (t,s)

-- |
-- @since 0.2.0.0
toCheckedEnc :: forall xs c str . (SymbolList xs) => Enc xs c str -> CheckedEnc c str 
toCheckedEnc (UnsafeMkEnc p c s) = 
        UnsafeMkCheckedEnc (symbolVals @ xs) c s   

-- |
-- @since 0.2.0.0
fromCheckedEnc :: forall xs c str . SymbolList xs => CheckedEnc c str -> Maybe (Enc xs c str)
fromCheckedEnc (UnsafeMkCheckedEnc xs c s) = 
    let p = Proxy :: Proxy xs
    in if symbolVals @ xs == xs
       then Just $ UnsafeMkEnc p c s
       else Nothing

------------------------

-- |
-- >>> let encsometest = UnsafeMkCheckedEnc ["TEST"] () $ T.pack "hello"
-- >>> procToCheckedEncFromCheckedEnc @'["TEST"] encsometest
-- True
-- >>> procToCheckedEncFromCheckedEnc @'["TEST1"] encsometest
-- False
procToCheckedEncFromCheckedEnc :: forall xs c str . (SymbolList xs, Eq c, Eq str) => CheckedEnc c str -> Bool
procToCheckedEncFromCheckedEnc x = (== Just x) . fmap (toCheckedEnc @ xs) . fromCheckedEnc $ x

-- |
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> procFromCheckedEncToCheckedEnc enctest
-- True
procFromCheckedEncToCheckedEnc :: forall xs c str . (SymbolList xs, Eq c, Eq str) => Enc xs c str -> Bool
procFromCheckedEncToCheckedEnc x = (== Just x) . fromCheckedEnc . toCheckedEnc $ x

-- |
-- >>> displ $ unsafeCheckedEnc ["TEST"] () ("hello" :: T.Text)
-- "UnsafeMkCheckedEnc [TEST] () (Text hello)"
instance (Show c, Displ str) => Displ (CheckedEnc c str) where
    displ (UnsafeMkCheckedEnc xs c s) = 
        "UnsafeMkCheckedEnc " ++ displ xs  ++ " " ++ show c ++ " " ++ displ s

