
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.TypedEncoding.Internal.Deprecated.Recreate where

import           Data.TypedEncoding.Internal.Class.Validate
import           Data.TypedEncoding.Internal.Types (Enc(..) 
                                              , toEncoding
                                              , withUnsafeCoerce
                                              , RecreateEx(..)
                                              , getPayload
                                             )
import           Data.TypedEncoding.Internal.Class.Util                                             
import           Data.Proxy
import           Data.Functor.Identity
import           GHC.TypeLits


-- | Used to safely recover encoded data validating all encodings
class RecreateF f instr outstr where    
    checkPrevF :: outstr -> f instr

class (Functor f) => RecreateFAll f (xs :: [Symbol]) c str where
    checkFAll :: Enc xs c str -> f (Enc '[] c str)
    recreateFAll :: Enc '[] c str -> f (Enc xs c str)
    recreateFAll str@(MkEnc _ _ pay) = 
        let str0 :: Enc xs c str = withUnsafeCoerce id str
        in withUnsafeCoerce (const pay) <$> checkFAll str0    

instance Applicative f => RecreateFAll f '[] c str where
    checkFAll (MkEnc _ c str) = pure $ toEncoding c str 


instance (Monad f, RecreateFAll f xs c str, RecreateF f (Enc xs c str) (Enc (x ': xs) c str)) => RecreateFAll f (x ': xs) c str where
    checkFAll str = 
        let re :: f (Enc xs c str) = checkPrevF str
        in re >>= checkFAll


recreateAll :: forall xs c str . RecreateFAll Identity xs c str => 
              Enc '[] c str 
              -> Enc xs c str
recreateAll = runIdentity . recreateFAll 


-- | Useful for partially manual recreation
recreateFPart_ :: forall f xs xsf c str . (Functor f, RecreateFAll f xs c str) => Proxy xs -> Enc xsf c str -> f (Enc (Append xs xsf) c str)
recreateFPart_ p (MkEnc _ conf str) = 
    let re :: f (Enc xs c str) = recreateFAll $ MkEnc Proxy conf str
    in  MkEnc Proxy conf . getPayload <$> re 


recreateFPart :: forall (xs :: [Symbol]) xsf f c str . (Functor f, RecreateFAll f xs c str) => Enc xsf c str -> f (Enc (Append xs xsf) c str)
recreateFPart = recreateFPart_ (Proxy :: Proxy xs) 


recreatePart_ :: RecreateFAll Identity (xs :: [Symbol]) c str => 
              Proxy xs 
              -> Enc xsf c str
              -> Enc (Append xs xsf) c str 
recreatePart_ p = runIdentity . recreateFPart_ p

recreatePart :: forall (xs :: [Symbol]) xsf c str . RecreateFAll Identity xs c str => 
               Enc xsf c str
              -> Enc (Append xs xsf) c str 
recreatePart = recreatePart_ (Proxy :: Proxy xs)   

