
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Combinators reexported in Data.TypedEncoding
module Data.TypedEncoding.Internal.Combinators where

import           Data.TypedEncoding.Internal.Types
import           Data.TypedEncoding.Internal.Class.Recreate
import           Data.TypedEncoding.Internal.Class.Util (SymbolList, Append)
import           GHC.TypeLits
import           Data.Proxy

-- $setup
-- >>> :set -XTypeApplications
-- >>> import qualified Data.Text as T
-- >>> import           Data.Word


-- * Partial application of encoding / decoding / recreation

-- | Any valid transformation of encodings (encoding / decoding / recreation) can be 
-- replayed on top of another encoding stack. 
--
-- This subsumes various /encPart, decPart, recrPart/ combinators.
aboveF :: forall (ts :: [Symbol]) xs ys f c str . (Functor f) =>
           (Enc xs c str -> f (Enc ys c str)) 
           -> Enc (Append xs ts) c str -> f (Enc (Append ys ts) c str)
aboveF fn (MkEnc _ conf str) = 
    let re :: f (Enc ys c str) = fn $ MkEnc Proxy conf str
    in  MkEnc Proxy conf . getPayload <$> re


above :: forall (ts :: [Symbol]) xs ys c str . 
           (Enc xs c str -> Enc ys c str) 
           -> Enc (Append xs ts) c str -> Enc (Append ys ts) c str
above fn (MkEnc _ conf str) = 
    let re ::Enc ys c str = fn $ MkEnc Proxy conf str
    in  MkEnc Proxy conf . getPayload $ re

-- * Converting 'UncheckedEnc' to 'Enc'

-- | Maybe signals annotation mismatch, effect @f@ is not evaluated unless there is match
verifyUncheckedEnc :: forall (xs :: [Symbol]) f c str . (
                     RecreateFAll f xs c str
                     , RecreateErr f
                     , Applicative f
                     , SymbolList xs
                   ) 
                   =>
                   UncheckedEnc c str
                   ->  Maybe (f (Enc xs c str))

verifyUncheckedEnc x = 
    -- let perr = Proxy :: Proxy "e-mismatch"
    --in  
      case verifyAnn @xs x of
            Left err -> Nothing -- asRecreateErr_ perr $ Left err
            Right (MkUncheckedEnc _ c str) -> Just $ recreateFAll . toEncoding c $ str


verifyUncheckedEnc' :: forall (xs :: [Symbol]) c str . (
                     RecreateFAll (Either RecreateEx) xs c str
                     , SymbolList xs
                   ) 
                   =>
                   UncheckedEnc c str
                   ->  Maybe (Either RecreateEx (Enc xs c str))
verifyUncheckedEnc' = verifyUncheckedEnc


