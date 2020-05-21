{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module defines 'CheckedEnc' - untyped ADT version of 'Enc' 

module Data.TypedEncoding.Common.Types.CheckedEnc where

import           Data.TypedEncoding.Internal.Enc
import           Data.TypedEncoding.Common.Types.Common
import           Data.TypedEncoding.Common.Class.Util
import           Data.Proxy

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T
-- >>> import  Data.TypedEncoding.Combinators.Unsafe (unsafeSetPayload)


-- * Untyped Enc

-- constructor is to be treated as Unsafe to Encode and Decode instance implementations
-- particular encoding instances may expose smart constructors for limited data types

-- | Represents some validated encoded string. 
--
-- @CheckedEnc@ is untyped version of 'Data.TypedEncoding.Internal.Enc.Enc'. 
-- @CheckedEnc@ contains verified encoded data, encoding is visible
-- at the value level only.
data CheckedEnc conf str = UnsafeMkCheckedEnc [EncAnn] conf str
     deriving (Show, Eq) 

unsafeCheckedEnc:: [EncAnn] -> c -> s -> CheckedEnc c s
unsafeCheckedEnc = UnsafeMkCheckedEnc

getCheckedPayload :: CheckedEnc conf str -> str
getCheckedPayload = snd . getCheckedEncPayload

getCheckedEncPayload :: CheckedEnc conf str -> ([EncAnn], str) 
getCheckedEncPayload (UnsafeMkCheckedEnc t _ s) = (t,s)

toCheckedEnc :: forall xs c str . (SymbolList xs) => Enc xs c str -> CheckedEnc c str 
toCheckedEnc (MkEnc p c s) = 
        UnsafeMkCheckedEnc (symbolVals @ xs) c s   


fromCheckedEnc :: forall xs c str . SymbolList xs => CheckedEnc c str -> Maybe (Enc xs c str)
fromCheckedEnc (UnsafeMkCheckedEnc xs c s) = 
    let p = Proxy :: Proxy xs
    in if symbolVals @ xs == xs
       then Just $ MkEnc p c s
       else Nothing

------------------------

-- |
-- >>> let encsometest = UnsafeMkCheckedEnc ["TEST"] () $ T.pack "hello"
-- >>> proc_toCheckedEncFromCheckedEnc @'["TEST"] encsometest
-- True
-- >>> proc_toCheckedEncFromCheckedEnc @'["TEST1"] encsometest
-- False
proc_toCheckedEncFromCheckedEnc :: forall xs c str . (SymbolList xs, Eq c, Eq str) => CheckedEnc c str -> Bool
proc_toCheckedEncFromCheckedEnc x = (== Just x) . fmap (toCheckedEnc @ xs) . fromCheckedEnc $ x

-- |
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> proc_fromCheckedEncToCheckedEnc enctest
-- True
proc_fromCheckedEncToCheckedEnc :: forall xs c str . (SymbolList xs, Eq c, Eq str) => Enc xs c str -> Bool
proc_fromCheckedEncToCheckedEnc x = (== Just x) . fromCheckedEnc . toCheckedEnc $ x

-- |
-- >>> displ $ unsafeCheckedEnc ["TEST"] () ("hello" :: T.Text)
-- "UnsafeMkCheckedEnc [TEST] () (Text hello)"
instance (Show c, Displ str) => Displ (CheckedEnc c str) where
    displ (UnsafeMkCheckedEnc xs c s) = 
        "UnsafeMkCheckedEnc " ++ displ xs  ++ " " ++ show c ++ " " ++ displ s

