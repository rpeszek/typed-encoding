
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Internal.Class.Encode where

import           Data.TypedEncoding.Internal.Class.Util

import           Data.TypedEncoding.Internal.Types (Enc(..) 
                                              , toEncoding
                                              , getPayload
                                              , withUnsafeCoerce
                                              , unsafeChangePayload
                                              , RecreateEx(..)
                                              , UnexpectedDecodeEx(..))
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits
import           Data.Semigroup ((<>))


class EncodeF f instr outstr where    
    encodeF :: instr -> f outstr

class EncodeFAll f (xs :: [Symbol]) c str where
    encodeFAll :: (Enc '[] c str) -> f (Enc xs c str)

instance Applicative f => EncodeFAll f '[] c str where
    encodeFAll (MkEnc _ c str) = pure $ toEncoding c str 

instance (Monad f, EncodeFAll f xs c str, EncodeF f (Enc xs c str) (Enc (x ': xs) c str)) => EncodeFAll f (x ': xs) c str where
    encodeFAll str = 
        let re :: f (Enc xs c str) = encodeFAll str
        in re >>= encodeF


encodeAll :: EncodeFAll Identity (xs :: [Symbol]) c str => 
              (Enc '[] c str) 
              -> (Enc xs c str)
encodeAll = runIdentity . encodeFAll             



encodeFPart_ :: forall f xs xsf c str . (Functor f, EncodeFAll f xs c str) => Proxy xs -> (Enc xsf c str) -> f (Enc (Append xs xsf) c str)
encodeFPart_ p (MkEnc _ conf str) = 
    let re :: f (Enc xs c str) = encodeFAll $ MkEnc Proxy conf str
    in  (MkEnc Proxy conf . getPayload) <$> re 


encodeFPart :: forall (xs :: [Symbol]) xsf f c str . (Functor f, EncodeFAll f xs c str) => (Enc xsf c str) -> f (Enc (Append xs xsf) c str)
encodeFPart = encodeFPart_ (Proxy :: Proxy xs) 


encodePart_ :: EncodeFAll Identity (xs :: [Symbol]) c str => 
              Proxy xs 
              -> (Enc xsf c str)
              -> (Enc (Append xs xsf) c str) 
encodePart_ p = runIdentity . encodeFPart_ p

-- | for some reason ApplyTypes syntax does not want to work if xs is specified with 
-- polymorphic [Symbol]
encodePart :: forall (xs :: [Symbol]) xsf c str . EncodeFAll Identity xs c str => 
               (Enc xsf c str)
              -> (Enc (Append xs xsf) c str) 
encodePart = encodePart_ (Proxy :: Proxy xs)              

