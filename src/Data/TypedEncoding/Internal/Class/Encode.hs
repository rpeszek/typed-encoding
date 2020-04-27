
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

class EncodeFAll f (xs :: [k]) c str where
    encodeFAll :: (Enc '[] c str) -> f (Enc xs c str)

instance Applicative f => EncodeFAll f '[] c str where
    encodeFAll (MkEnc _ c str) = pure $ toEncoding c str 

instance (Monad f, EncodeFAll f xs c str, EncodeF f (Enc xs c str) (Enc (x ': xs) c str)) => EncodeFAll f (x ': xs) c str where
    encodeFAll str = 
        let re :: f (Enc xs c str) = encodeFAll str
        in re >>= encodeF


encodeAll :: EncodeFAll Identity (xs :: [k]) c str => 
              (Enc '[] c str) 
              -> (Enc xs c str)
encodeAll = runIdentity . encodeFAll             



encodeFPart :: forall f xs xsf c str . (Functor f, EncodeFAll f xs c str) => Proxy xs -> (Enc xsf c str) -> f (Enc (Append xs xsf) c str)
encodeFPart p (MkEnc _ conf str) = 
    let re :: f (Enc xs c str) = encodeFAll $ MkEnc Proxy conf str
    in  (MkEnc Proxy conf . getPayload) <$> re 


encodeFPart_ :: forall (xs :: [Symbol]) xsf f c str . (Functor f, EncodeFAll f xs c str) => (Enc xsf c str) -> f (Enc (Append xs xsf) c str)
encodeFPart_ = encodeFPart (Proxy :: Proxy xs) 


encodePart :: EncodeFAll Identity (xs :: [k]) c str => 
              Proxy xs 
              -> (Enc xsf c str)
              -> (Enc (Append xs xsf) c str) 
encodePart p = runIdentity . encodeFPart p

-- | for some reason ApplyTypes syntax does not want to work if xs is specified with 
-- polymorphic [k]
encodePart_ :: forall (xs :: [Symbol]) xsf c str . EncodeFAll Identity xs c str => 
               (Enc xsf c str)
              -> (Enc (Append xs xsf) c str) 
encodePart_ = encodePart (Proxy :: Proxy xs)              

