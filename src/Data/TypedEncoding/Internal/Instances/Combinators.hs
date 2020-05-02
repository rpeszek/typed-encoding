
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.TypedEncoding.Internal.Instances.Combinators where

import           Data.String
import           Data.Proxy
import           Text.Read
import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Class.IsStringR 
import           GHC.TypeLits

-- $setup
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.Word


-- * Composite encodings from 'Foldable' 'Functor' types

-- | allows to fold payload in Enc to create another Enc, assumes homogenious input encodings.
foldEnc :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) f c s1 s2 
           . (Foldable f, Functor f) 
           => c -> (s1 -> s2 -> s2) -> s2 -> f (Enc xs1 c s1) -> Enc xs2 c s2
foldEnc c f sinit = unsafeSetPayload c . foldr f sinit . fmap getPayload 

-- | Similar to 'foldEnc', assumes that destination payload has @IsString@ instance and uses @""@ as base case. 
foldEncStr :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) f c s1 s2 
             . (Foldable f, Functor f, IsString s2) 
             => c -> (s1 -> s2 -> s2) -> f (Enc xs1 c s1) -> Enc xs2 c s2
foldEncStr c f = foldEnc c f ""

-- TODO pass SomeAnn to fold
-- | Similar to 'foldEnc', works with untyped 'SomeEnc'
foldSomeEnc :: forall (xs2 :: [Symbol]) f c s1 s2 
             . (Foldable f, Functor f) 
             => c -> ([SomeAnn] -> s1 -> s2 -> s2) -> s2 -> f (SomeEnc c s1) -> Enc xs2 c s2
foldSomeEnc c f sinit = unsafeSetPayload c . foldr (uncurry f) sinit . fmap getSomeEncPayload

-- | Similar to 'foldEncStr', works with untyped 'SomeEnc'
foldSomeEncStr :: forall (xs2 :: [Symbol]) f c s1 s2 . (Foldable f, Functor f, IsString s2) => c -> ([SomeAnn] -> s1 -> s2 -> s2) -> f (SomeEnc c s1) -> Enc xs2 c s2
foldSomeEncStr c f  = foldSomeEnc c f ""


-- * Composite encoding: Recreate and Encode helpers

-- | Splits composite payload into homogenious chunks
splitPayload :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) c s1 s2 . 
             (s1 -> [s2]) 
             -> Enc xs1 c s1 
             -> [Enc xs2 c s2]
splitPayload f (MkEnc _ c s1) = map (MkEnc Proxy c) (f s1)
   
-- | Untyped version of 'splitPayload'
splitSomePayload :: forall c s1 s2 . 
             ([SomeAnn] -> s1 -> [([SomeAnn], s2)]) 
             -> SomeEnc c s1 
             -> [SomeEnc c s2]
splitSomePayload f (MkSomeEnc ann1 c s1) = map (\(ann2, s2) -> MkSomeEnc ann2 c s2) (f ann1 s1)


-- * Utility combinators 

-- | sometimes show . read is not identity, eg. Word8:
--
-- >>> read "256" :: Word8
-- 0
--
-- >>> verifyWithRead @Word8 "Word8-decimal" (T.pack "256")
-- Left "Payload does not satisfy format Word8-decimal: 256"
-- >>> verifyWithRead @Word8 "Word8-decimal" (T.pack "123")
-- Right "123"
verifyWithRead :: forall a str . (IsStringR str, IsString str, Read a, Show a) => String -> str -> Either String str
verifyWithRead msg x = 
    let s = toString x
        a :: Maybe a = readMaybe s
        check = (show <$> a) == Just s 
    in if check
       then Right x
       else Left $ "Payload does not satisfy format " ++ msg ++ ": " ++ s    
