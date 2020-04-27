
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

module Data.TypedEncoding.Internal.Class.Recreate where

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


-- | Used to safely recover encoded data validating all encodingss
class RecreateF f instr outstr where    
    checkPrevF :: outstr -> f instr

class (Functor f) => RecreateFAll f (xs :: [k]) c str where
    checkFAll :: (Enc xs c str) -> f (Enc '[] c str)
    recreateFAll :: (Enc '[] c str) -> f (Enc xs c str)
    recreateFAll str@(MkEnc _ _ pay) = 
        let str0 :: Enc xs c str = withUnsafeCoerce id str
        in fmap (withUnsafeCoerce (const pay)) $ checkFAll str0    

instance Applicative f => RecreateFAll f '[] c str where
    checkFAll (MkEnc _ c str) = pure $ toEncoding c str 


instance (Monad f, RecreateFAll f xs c str, RecreateF f (Enc xs c str) (Enc (x ': xs) c str)) => RecreateFAll f (x ': xs) c str where
    checkFAll str = 
        let re :: f (Enc xs c str) = checkPrevF str
        in re >>= checkFAll


recreateAll :: RecreateFAll Identity (xs :: [k]) c str => 
              (Enc '[] c str) 
              -> (Enc xs c str)
recreateAll = runIdentity . recreateFAll 

-- TODO using RecreateErr typeclass is overkill

-- | Recovery errors are expected unless Recovery allows Identity instance
class RecreateErr f where 
    recoveryErr :: RecreateEx -> f a

instance RecreateErr (Either RecreateEx) where
    recoveryErr = Left  

asRecreateErr :: (RecreateErr f, Applicative f, Show err, KnownSymbol x) => Proxy x -> Either err a -> f a
asRecreateErr p (Left err) = recoveryErr $ RecreateEx p err
asRecreateErr _ (Right r) = pure r
