{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Internal definition of types

module Data.TypedEncoding.Internal.Types.CheckedEnc where

import           Data.TypedEncoding.Internal.Types.Enc

import           Data.Proxy
import           Data.TypedEncoding.Internal.Class.Util
import           Data.TypedEncoding.Internal.Types.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T


-- * Untyped Enc

-- constructor is to be treated as Unsafe to Encode and Decode instance implementations
-- particular encoding instances may expose smart constructors for limited data types

-- | Represents some validated encoded string. 
--
-- @CheckedEnc@ is untyped version of 'Data.TypedEncoding.Internal.Types.Enc.Enc'. 
-- @CheckedEnc@ contains verified encoded data, encoding is visible
-- at the value level only.
data CheckedEnc conf str = MkCheckedEnc [EncAnn] conf str
     deriving (Show, Eq) 

unsafeCheckedEnc:: [EncAnn] -> c -> s -> CheckedEnc c s
unsafeCheckedEnc = MkCheckedEnc

getCheckedPayload :: CheckedEnc conf str -> str
getCheckedPayload = snd . getCheckedEncPayload

getCheckedEncPayload :: CheckedEnc conf str -> ([EncAnn], str) 
getCheckedEncPayload (MkCheckedEnc t _ s) = (t,s)

toCheckedEnc :: forall xs c str . (KnownAnnotation xs) => Enc xs c str -> CheckedEnc c str 
toCheckedEnc (MkEnc p c s) = 
        MkCheckedEnc (knownAnn @ xs) c s   


fromCheckedEnc :: forall xs c str . KnownAnnotation xs => CheckedEnc c str -> Maybe (Enc xs c str)
fromCheckedEnc (MkCheckedEnc xs c s) = 
    let p = Proxy :: Proxy xs
    in if knownAnn @ xs == xs
       then Just $ MkEnc p c s
       else Nothing


-- | Existentially quantified quanitified @Enc@
data SomeEnc conf str where
    MkSomeEnc :: Enc xs conf str -> SomeEnc conf str
    
-- fromSomeEnc :: forall xs c s . SomeEnc c s -> Enc xs c s
-- fromSomeEnc (MkSomeEnc enc) = enc

withSomeEnc :: SomeEnc conf str -> (forall xs . Enc xs conf str -> r) -> r
withSomeEnc (MkSomeEnc enc) f = f enc


-- |
-- >>> let encsometest = MkCheckedEnc ["TEST"] () $ T.pack "hello"
-- >>> proc_toCheckedEncFromCheckedEnc @'["TEST"] encsometest
-- True
-- >>> proc_toCheckedEncFromCheckedEnc @'["TEST1"] encsometest
-- False
proc_toCheckedEncFromCheckedEnc :: forall xs c str . (KnownAnnotation xs, Eq c, Eq str) => CheckedEnc c str -> Bool
proc_toCheckedEncFromCheckedEnc x = (== Just x) . fmap (toCheckedEnc @ xs) . fromCheckedEnc $ x

-- |
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> proc_fromCheckedEncToCheckedEnc enctest
-- True
proc_fromCheckedEncToCheckedEnc :: forall xs c str . (KnownAnnotation xs, Eq c, Eq str) => Enc xs c str -> Bool
proc_fromCheckedEncToCheckedEnc x = (== Just x) . fromCheckedEnc . toCheckedEnc $ x

-- |
-- >>> displ $ MkCheckedEnc ["TEST"] () ("hello" :: T.Text)
-- "MkCheckedEnc [TEST] () (Text hello)"
instance (Show c, Displ str) => Displ (CheckedEnc c str) where
    displ (MkCheckedEnc xs c s) = 
        "MkCheckedEnc " ++ displ xs  ++ " " ++ show c ++ " " ++ displ s
