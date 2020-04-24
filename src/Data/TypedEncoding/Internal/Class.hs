
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Data.TypedEncoding.Internal.Class where

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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Data.List as L


-- $setup
-- >>> :set -XScopedTypeVariables -XKindSignatures -XMultiParamTypeClasses -XDataKinds -XPolyKinds -XFlexibleInstances -XFlexibleContexts
-- >>> import Data.TypedEncoding.Internal.Types (unsafeSetPayload)

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



class DecodeF f instr outstr where    
    decodeF :: instr -> f outstr

class DecodeFAll f (xs :: [k]) c str where
    decodeFAll :: (Enc xs c str) ->  f (Enc '[] c str)

instance Applicative f => DecodeFAll f '[] c str where
    decodeFAll (MkEnc _ c str) = pure $ toEncoding c str 

instance (Monad f, DecodeFAll f xs c str, DecodeF f (Enc (x ': xs) c str) (Enc (xs) c str)) => DecodeFAll f (x ': xs) c str where
    decodeFAll str = 
        let re :: f (Enc xs c str) = decodeF str
        in re >>= decodeFAll

decodeAll :: DecodeFAll Identity (xs :: [k]) c str => 
              (Enc xs c str)
              -> (Enc '[] c str) 
decodeAll = runIdentity . decodeFAll 

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

                    

-- | TODO use singletons definition instead?
type family Append (xs :: [k]) (ys :: [k]) :: [k] where
    Append '[] xs = xs
    Append (y ': ys) xs = y ': (Append ys xs)

encodeFPart :: forall f xs xsf c str . (Functor f, EncodeFAll f xs c str) => Proxy xs -> (Enc xsf c str) -> f (Enc (Append xs xsf) c str)
encodeFPart p (MkEnc _ conf str) = 
    let re :: f (Enc xs c str) = encodeFAll $ MkEnc Proxy conf str
    in  (MkEnc Proxy conf . getPayload) <$> re 

encodePart :: EncodeFAll Identity (xs :: [k]) c str => 
              Proxy xs 
              -> (Enc xsf c str)
              -> (Enc (Append xs xsf) c str) 
encodePart p = runIdentity . encodeFPart p

-- | Unsafe implementation guarded by safe type definition
decodeFPart :: forall f xs xsf c str . (Functor f, DecodeFAll f xs c str) => Proxy xs -> (Enc (Append xs xsf) c str) -> f (Enc xsf c str)
decodeFPart p (MkEnc _ conf str) = 
    let re :: f (Enc '[] c str) = decodeFAll $ MkEnc (Proxy :: Proxy xs) conf str
    in  (MkEnc Proxy conf . getPayload) <$> re 
 
decodePart :: DecodeFAll Identity (xs :: [k]) c str => 
              Proxy xs 
              -> (Enc (Append xs xsf) c str) 
              -> (Enc xsf c str) 
decodePart p = runIdentity . decodeFPart p

-- Other classes --

-- subsets are useful for restriction encodings
-- like r-UFT8 but not for other encodings.
class Subset (x :: k) (y :: k) where
    inject :: Proxy y -> Enc (x ': xs) c str ->  Enc (y ': xs) c str
    inject _ = withUnsafeCoerce id

class FlattenAs (x :: k) (y :: k) where
    flattenAs :: Proxy y -> Enc (x ': xs) c str ->  Enc '[y] c str
    flattenAs _ = withUnsafeCoerce id

-- | Polymorphic data payloads used to encode/decode
class HasA c a where
    has :: Proxy a -> c -> a

instance HasA a () where
    has _ = const ()

-- | With type safety in pace decoding errors should be unexpected
-- this class can be used to provide extra info if decoding could fail
class UnexpectedDecodeErr f where 
    unexpectedDecodeErr :: UnexpectedDecodeEx -> f a

instance UnexpectedDecodeErr Identity where
    unexpectedDecodeErr x = fail $ show x

instance UnexpectedDecodeErr (Either UnexpectedDecodeEx) where
    unexpectedDecodeErr = Left 

asUnexpected :: (UnexpectedDecodeErr f, Applicative f, Show err, KnownSymbol x) => Proxy x -> Either err a -> f a
asUnexpected p (Left err) = unexpectedDecodeErr $ UnexpectedDecodeEx p err
asUnexpected _ (Right r) = pure r

-- TODO using RecreateErr typeclass is overkill

-- | Recovery errors are expected unless Recovery allows Identity instance
class RecreateErr f where 
    recoveryErr :: RecreateEx -> f a

instance RecreateErr (Either RecreateEx) where
    recoveryErr = Left  

asRecreateErr :: (RecreateErr f, Applicative f, Show err, KnownSymbol x) => Proxy x -> Either err a -> f a
asRecreateErr p (Left err) = recoveryErr $ RecreateEx p err
asRecreateErr _ (Right r) = pure r


-- * Display 

-- | Human friendly version of Show
class Displ x where 
    displ :: x -> String

instance Displ String where
    displ = id    
instance Displ T.Text where
    displ x = "(Text " ++ T.unpack x ++ ")"
instance Displ TL.Text where
    displ x = "(TL.Text " ++ TL.unpack x ++ ")"
instance Displ B.ByteString where
    displ x = "(ByteString " ++ B.unpack x ++ ")" 
instance Displ BL.ByteString where
    displ x = "(ByteString " ++ BL.unpack x ++ ")" 


instance Displ (Proxy '[]) where
    displ _ = ""

-- |
-- >>> displ (Proxy :: Proxy ["FIRST", "SECOND"])
-- "FIRST,SECOND"
instance (pxs ~ Proxy xs, Displ pxs, KnownSymbol x) => Displ (Proxy (x ': xs)) where
    displ _ =  L.dropWhileEnd (',' ==) $  symbolVal (Proxy :: Proxy x) ++ "," ++ displ (Proxy :: Proxy xs)

-- >>> let disptest = unsafeSetPayload () "hello" :: Enc '["TEST"] () T.Text
-- >>> displ disptest
-- "MkEnc '[TEST] () hello"
instance (Displ (Proxy xs), Show c, Displ str) => Displ ( Enc xs c str) where
    displ (MkEnc p c s) = 
        "MkEnc '[" ++ displ p ++ "] " ++ show c ++ " " ++ displ s


-- Utils --

errorOnLeft :: Show err => Either err a -> a
errorOnLeft (Left e) = error $ "You trusted encodings too much " <> show e
errorOnLeft (Right r) =  r
