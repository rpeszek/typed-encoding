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

module Data.TypedEncoding.Internal.Types (
        module Data.TypedEncoding.Internal.Types
        , type SomeAnn
    ) where

import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits
import           Data.TypedEncoding.Internal.Class.Util

-- $setup
-- >>> :set -XOverloadedStrings -XMultiParamTypeClasses -XDataKinds -XAllowAmbiguousTypes
-- >>> import qualified Data.Text as T

-- Not a Functor on purpose
data Enc enc conf str where
    -- | constructor is to be treated as Unsafe to Encode and Decode instance implementations
    -- particular encoding instances may expose smart constructors for limited data types
    MkEnc :: Proxy enc -> conf -> str -> Enc enc conf str
    deriving (Show, Eq) 

-- |
-- >>> let disptest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ disptest
-- "MkEnc '[TEST] () (Text hello)"
instance (KnownAnnotation xs, Show c, Displ str) => Displ ( Enc xs c str) where
    displ (MkEnc p c s) = 
        "MkEnc '[" ++ knownAnn @ xs ++ "] " ++ show c ++ " " ++ displ s


toEncoding :: conf -> str -> Enc '[] conf str
toEncoding conf str = MkEnc Proxy conf str

fromEncoding :: Enc '[] conf str -> str
fromEncoding = getPayload

-- TODO make all implTran functions module-private
-- TODO disambiguate implEncode from implDecode, from implCheckPrevF for type safety
-- especially since these are always used in combo with asRecreateErr or asUnexpected 

implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (\c -> f)

-- TODO could this type be more precise?
implEncodeF_ :: (Show err, KnownSymbol x) => Proxy x -> (str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF_ p f = implTranF (either (Left . EncodeEx p) Right . f) 

implEncodeF :: forall x enc1 enc2 err conf str . 
              (Show err, KnownSymbol x) 
              => (str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF = implEncodeF_ (Proxy :: Proxy x)

implDecodeF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implDecodeF = implTranF

implCheckPrevF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implCheckPrevF = implTranF


implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (MkEnc _ conf str) = (MkEnc Proxy conf) <$> f conf str

implEncodeF_' :: (Show err, KnownSymbol x) => Proxy x -> (conf -> str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF_' p f = implTranF' (\c -> either (Left . EncodeEx p) Right . f c) 

implDecodeF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implDecodeF' = implTranF'

implTranP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP f  = implTranF' (\c -> pure . f)

implEncodeP :: Applicative f => (str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implEncodeP = implTranP

implTranP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranP' f  = implTranF' (\c -> pure . f c)

implEncodeP' :: Applicative f => (conf -> str -> str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implEncodeP' = implTranP'


getPayload :: Enc enc conf str -> str  
getPayload (MkEnc _ _ str) = str

unsafeSetPayload :: conf -> str -> Enc enc conf str 
unsafeSetPayload c str = MkEnc Proxy c str

withUnsafeCoerce ::  (s1 -> s2) -> Enc e1 c s1 -> Enc e2 c s2
withUnsafeCoerce f (MkEnc _ conf str)  = (MkEnc Proxy conf (f str)) 

unsafeChangePayload ::  (s1 -> s2) -> Enc e c s1 -> Enc e c s2
unsafeChangePayload f (MkEnc p conf str)  = (MkEnc p conf (f str)) 

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


-- * Unchecked for recreate, similar to SomeEnc only not verified

data Unchecked c str = MkUnchecked SomeAnn c str deriving (Show, Eq)

toUnchecked :: SomeAnn -> c -> str -> Unchecked c str
toUnchecked = MkUnchecked

getUncheckedAnn :: Unchecked c str -> SomeAnn
getUncheckedAnn (MkUnchecked ann _ _) = ann

verifyAnn :: forall xs c str . KnownAnnotation xs => Unchecked c str -> Either String (Unchecked c str)
verifyAnn x@(MkUnchecked xs _ _) = 
    let p = Proxy :: Proxy xs
    in if "[" ++ knownAnn @ xs ++ "]" == xs
       then Right $ x
       else Left $ "Unchecked has not matching annotation " ++ xs

-- TODO verifyUnchecked in Recrete


-- | Represents errors in recovery (recreation of encoded types).
data RecreateEx where
    RecreateEx:: (Show e, KnownSymbol x) => Proxy x -> e -> RecreateEx
    RecreateExUnkStep::   (Show e) => e -> RecreateEx

instance Show RecreateEx where
    show (RecreateEx prxy a) = "(RecreateEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"
    show (RecreateExUnkStep  a) = "(UnknownDecodeStep (" ++ show a ++ "))"


recreateErrUnknown :: (Show e) => e -> RecreateEx
recreateErrUnknown  = RecreateExUnkStep

-- instance Eq RecreateEx where
--     (RecreateEx prxy1 a1) == RecreateEx prxy2 a2 = (symbolVal prxy1) == (symbolVal prxy2)


-- | Represents errors in encoding
data EncodeEx where
    EncodeEx:: (Show a, KnownSymbol x) => Proxy x -> a -> EncodeEx 

instance Show EncodeEx where
    show (EncodeEx prxy a) = "(EncodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"



-- | Type safety over encodings makes decoding process safe.
-- However failures are still possible due to bugs or unsafe payload modifications.
-- UnexpectedDecodeEx represents such errors.
data UnexpectedDecodeEx where
    UnexpectedDecodeEx :: (Show a, KnownSymbol x) => Proxy x -> a -> UnexpectedDecodeEx

instance Show UnexpectedDecodeEx where
    show (UnexpectedDecodeEx prxy a) = "(UnexpectedDecodeEx \"" ++ symbolVal prxy ++ "\" (" ++ show a ++ "))"
