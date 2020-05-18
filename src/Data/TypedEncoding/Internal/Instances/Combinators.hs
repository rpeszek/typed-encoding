
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Combinators that can be helpful in instance creation.
module Data.TypedEncoding.Internal.Instances.Combinators where

import           Data.String
import           Data.Proxy
import           Text.Read
import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Class.IsStringR 
import           GHC.TypeLits

-- TODO v0.3 some of this needs to move to call side exported combinators

-- $setup
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.Word


-- * Composite encodings from 'Foldable' 'Functor' types

-- | allows to fold payload in Enc to create another Enc, assumes homogeneous input encodings.
-- This yields not a type safe code, better implementation code should use fixed size
-- dependently typed @Vect n@ or some @HList@ like foldable.
foldEnc :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) f c s1 s2 
           . (Foldable f, Functor f) 
           => c -> (s1 -> s2 -> s2) -> s2 -> f (Enc xs1 c s1) -> Enc xs2 c s2
foldEnc c f sinit = unsafeSetPayload c . foldr f sinit . fmap getPayload 

-- | Similar to 'foldEnc', assumes that destination payload has @IsString@ instance and uses @""@ as base case. 
foldEncStr :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) f c s1 s2 
             . (Foldable f, Functor f, IsString s2) 
             => c -> (s1 -> s2 -> s2) -> f (Enc xs1 c s1) -> Enc xs2 c s2
foldEncStr c f = foldEnc c f ""

-- | Similar to 'foldEnc', works with untyped 'CheckedEnc'
foldCheckedEnc :: forall (xs2 :: [Symbol]) f c s1 s2 
             . (Foldable f, Functor f) 
             => c -> ([EncAnn] -> s1 -> s2 -> s2) -> s2 -> f (CheckedEnc c s1) -> Enc xs2 c s2
foldCheckedEnc c f sinit = unsafeSetPayload c . foldr (uncurry f) sinit . fmap getCheckedEncPayload

-- | Similar to 'foldEncStr', works with untyped 'CheckedEnc'
foldCheckedEncStr :: forall (xs2 :: [Symbol]) f c s1 s2 . (Foldable f, Functor f, IsString s2) => c -> ([EncAnn] -> s1 -> s2 -> s2) -> f (CheckedEnc c s1) -> Enc xs2 c s2
foldCheckedEncStr c f  = foldCheckedEnc c f ""


-- * Composite encoding: Recreate and Encode helpers

-- | Splits composite payload into homogenious chunks
splitPayload :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) c s1 s2 . 
             (s1 -> [s2]) 
             -> Enc xs1 c s1 
             -> [Enc xs2 c s2]
splitPayload f (MkEnc _ c s1) = map (MkEnc Proxy c) (f s1)
   
-- | Untyped version of 'splitPayload'
splitSomePayload :: forall c s1 s2 . 
             ([EncAnn] -> s1 -> [([EncAnn], s2)]) 
             -> CheckedEnc c s1 
             -> [CheckedEnc c s2]
splitSomePayload f (MkCheckedEnc ann1 c s1) = map (\(ann2, s2) -> MkCheckedEnc ann2 c s2) (f ann1 s1)


-- * Utility combinators 

-- | sometimes show . read is not identity, eg. Word8:
--
-- >>> read "256" :: Word8
-- 0
--
-- >>> verifyWithRead @Word8 "Word8-decimal" (T.pack "256")
-- Left "Payload does not satisfy format Word8-decimal: 256"
--
-- >>> verifyWithRead @Word8 "Word8-decimal" (T.pack "123")
-- Right "123"
verifyWithRead :: forall a str . (IsStringR str, Read a, Show a) => String -> str -> Either String str
verifyWithRead msg x = 
    let s = toString x
        a :: Maybe a = readMaybe s
        check = (show <$> a) == Just s 
    in if check
       then Right x
       else Left $ "Payload does not satisfy format " ++ msg ++ ": " ++ s    


-- | Convenience function for checking if @str@ decodes properly
-- using @enc@ encoding markers and decoders that can pick decoder based
-- on that marker
verifyDynEnc :: forall s str err1 err2 enc a. (KnownSymbol s, Show err1, Show err2) => 
                  Proxy s   -- ^ proxy defining encoding annotation
                  -> (Proxy s -> Either err1 enc)  -- ^ finds encoding marker @enc@ for given annotation or fails
                  -> (enc -> str -> Either err2 a)  -- ^ decoder based on @enc@ marker
                  -> str                            -- ^ input
                  -> Either EncodeEx str
verifyDynEnc p findenc decoder str = 
  do
    enc <- asEncodeEx p . findenc $ p
    case decoder enc str of
      Left err -> Left $ EncodeEx p err
      Right r -> Right str


-- TODO make all implTran functions module-private
-- TODO disambiguate implEncode from implDecode, from implCheckPrevF for type safety
-- especially since these are always used in combo with asRecreateErr_ or asUnexpected 

implTranF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF f  = implTranF' (const f)


implDecodeF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implDecodeF = implTranF

implCheckPrevF :: Functor f => (str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implCheckPrevF = implTranF


implTranF' :: Functor f =>  (conf -> str -> f str) -> Enc enc1 conf str -> f (Enc enc2 conf str)
implTranF' f (MkEnc _ conf str) = MkEnc Proxy conf <$> f conf str


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

implChangeAnn :: Functor f => (Enc enc1 conf str -> f (Enc enc2a conf str)) -> Enc enc1 conf str -> f (Enc enc2b conf str)
implChangeAnn fn = fmap (withUnsafeCoerce id) . fn

-- TODO could this type be more precise?
implEncodeF_ :: (Show err, KnownSymbol x) => Proxy x -> (str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF_ p f = implTranF (either (Left . EncodeEx p) Right . f) 

implEncodeF :: forall x enc1 enc2 err conf str . 
              (Show err, KnownSymbol x) 
              => (str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF = implEncodeF_ (Proxy :: Proxy x)

implEncodeF_' :: (Show err, KnownSymbol x) => Proxy x -> (conf -> str -> Either err str) ->  Enc enc1 conf str -> Either EncodeEx (Enc enc2 conf str) 
implEncodeF_' p f = implTranF' (\c -> either (Left . EncodeEx p) Right . f c) 
