{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

module Data.TypedEncoding.Internal.Types.SomeEnc where

import           Data.TypedEncoding.Internal.Types.Enc

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits
import           Data.TypedEncoding.Internal.Class.Util
import           Data.TypedEncoding.Internal.Types.Common

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T


-- * Untyped Enc

-- | Untyped version of Enc. SomeEnc contains some verfied encoding, encoding is visible
-- at value level only.
data SomeEnc conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkSomeEnc :: SomeAnn -> conf -> str -> SomeEnc conf str
    deriving (Show, Eq) 

unsafeSomeEnc:: SomeAnn -> c -> s -> SomeEnc c s
unsafeSomeEnc = MkSomeEnc

getSomePayload :: SomeEnc conf str -> str
getSomePayload = snd . getSomeEncPayload

getSomeEncPayload :: SomeEnc conf str -> (SomeAnn, str) 
getSomeEncPayload (MkSomeEnc t _ s) = (t,s)

toSomeEnc :: forall xs c str . (KnownAnnotation xs) => Enc xs c str -> SomeEnc c str 
toSomeEnc (MkEnc p c s) = 
        MkSomeEnc ("[" ++ knownAnn @ xs ++ "]") c s   


fromSomeEnc :: forall xs c str . KnownAnnotation xs => SomeEnc c str -> Maybe (Enc xs c str)
fromSomeEnc (MkSomeEnc xs c s) = 
    let p = Proxy :: Proxy xs
    in if "[" ++ knownAnn @ xs ++ "]" == xs
       then Just $ MkEnc p c s
       else Nothing


-- TODO this does
-- data SomeEnc' conf str where
--     -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
--     -- particular encoding instances may expose smart constructors for limited data types
--     MkSomeEnc' :: Enc xs conf str -> SomeEnc' conf str
    


-- withSomeEnc' :: SomeEnc' conf str -> (forall xs . Enc xs conf str -> r) -> r
-- withSomeEnc' (MkSomeEnc' enc) f = f enc


-- |
-- >>> let encsometest = MkSomeEnc "[TEST]" () $ T.pack "hello"
-- >>> proc_toSomeEncFromSomeEnc @'["TEST"] encsometest
-- True
-- >>> proc_toSomeEncFromSomeEnc @'["TEST1"] encsometest
-- False
proc_toSomeEncFromSomeEnc :: forall xs c str . (KnownAnnotation xs, Eq c, Eq str) => SomeEnc c str -> Bool
proc_toSomeEncFromSomeEnc x = (== Just x) . fmap (toSomeEnc @ xs) . fromSomeEnc $ x

-- |
-- >>> let enctest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> proc_fromSomeEncToSomeEnc enctest
-- True
proc_fromSomeEncToSomeEnc :: forall xs c str . (KnownAnnotation xs, Eq c, Eq str) => Enc xs c str -> Bool
proc_fromSomeEncToSomeEnc x = (== Just x) . fromSomeEnc . toSomeEnc $ x

instance (Show c, Displ str) => Displ (SomeEnc c str) where
    displ (MkSomeEnc xs c s) = 
        "MkSomeEnc " ++ xs ++ show c ++ " " ++ displ s


