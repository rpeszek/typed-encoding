
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Various helper functions.
-- There are mostly for for creating @ToEncString@ and @FromEncString@ instances

module Data.TypedEncoding.Instances.Support.Helpers where

-- import           Data.String
import           Data.Proxy
import           Text.Read
import           Data.TypedEncoding.Common.Types
import           Data.TypedEncoding.Combinators.Unsafe
import           Data.TypedEncoding.Common.Class.IsStringR 
import           GHC.TypeLits


-- $setup
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.Word


-- * Composite encodings from 'Foldable' 'Functor' types

-- | allows to fold payload in Enc to create another Enc, assumes homogeneous input encodings.
-- This yields not a type safe code, better implementation code should use fixed size
-- dependently typed @Vect n@ or some @HList@ like foldable.
--
-- @since 0.2.0.0
foldEnc :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) f c s1 s2 
           . (Foldable f, Functor f) 
           => c -> (s1 -> s2 -> s2) -> s2 -> f (Enc xs1 c s1) -> Enc xs2 c s2
foldEnc c f sinit = unsafeSetPayload c . foldr f sinit . fmap getPayload 


-- | Similar to 'foldEnc', works with untyped 'CheckedEnc'
--
-- @since 0.2.0.0
foldCheckedEnc :: forall (xs2 :: [Symbol]) f c s1 s2 
             . (Foldable f, Functor f) 
             => c -> ([EncAnn] -> s1 -> s2 -> s2) -> s2 -> f (CheckedEnc c s1) -> Enc xs2 c s2
foldCheckedEnc c f sinit = unsafeSetPayload c . foldr (uncurry f) sinit . fmap getCheckedEncPayload



-- * Composite encoding: Recreate and Encode helpers

-- | Splits composite payload into homogeneous chunks
--
-- @since 0.2.0.0
splitPayload :: forall (xs2 :: [Symbol]) (xs1 :: [Symbol]) c s1 s2 . 
             (s1 -> [s2]) 
             -> Enc xs1 c s1 
             -> [Enc xs2 c s2]
splitPayload f (UnsafeMkEnc _ c s1) = map (UnsafeMkEnc Proxy c) (f s1)
   
-- | Untyped version of 'splitPayload'
--
-- (renamed from @splitCheckedPayload@ in previous versions)
-- @since 0.5.0.0
splitCheckedPayload :: forall c s1 s2 . 
             ([EncAnn] -> s1 -> [([EncAnn], s2)]) 
             -> CheckedEnc c s1 
             -> [CheckedEnc c s2]
splitCheckedPayload f (UnsafeMkCheckedEnc ann1 c s1) = map (\(ann2, s2) -> UnsafeMkCheckedEnc ann2 c s2) (f ann1 s1)


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
--
-- @since 0.2.0.0
verifyWithRead :: forall a str . (IsStringR str, Read a, Show a) => String -> str -> Either String str
verifyWithRead msg x = 
    let s = toString x
        a :: Maybe a = readMaybe s
        check = (show <$> a) == Just s 
    in if check
       then Right x
       else Left $ "Payload does not satisfy format " ++ msg ++ ": " ++ s    


-- | Convenience function for checking if @str@ decodes without error
-- using @dec@ encoding markers and decoders that can pick decoder based
-- on that marker
--
-- @since 0.3.0.0
verifyDynEnc :: forall s str err1 err2 dec a. (KnownSymbol s, Show err1, Show err2) => 
                  Proxy s   -- ^ proxy defining encoding annotation
                  -> (Proxy s -> Either err1 dec)  -- ^ finds encoding marker @dec@ for given annotation or fails
                  -> (dec -> str -> Either err2 a)  -- ^ decoder based on @dec@ marker
                  -> str                            -- ^ input
                  -> Either EncodeEx str
verifyDynEnc p findenc decoder str = 
  do
    enc <- asEncodeEx p . findenc $ p
    case decoder enc str of
      Left err -> Left $ EncodeEx p err
      Right r -> Right str


