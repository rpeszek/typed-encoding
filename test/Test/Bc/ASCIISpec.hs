{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | Verified backward compatibility of ASCII encoder changes in v0.3
module Test.Bc.ASCIISpec where

import qualified Data.TypedEncoding.Instances.Restriction.ASCII as ASCII
import           Data.TypedEncoding.Class.Util.StringConstraints


-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.List as L

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8

import           Data.Char
import           Control.Arrow

import           Test.QuickCheck.Instances.ByteString ()
import           Test.QuickCheck.Instances.Text ()
import           Test.QuickCheck
import           Test.QuickCheck.Property
import           Test.Hspec


equivalProperty :: Eq str => (str -> Either NonAsciiChar str) -> (str -> Either NonAsciiChar str) -> str -> Property
equivalProperty alg1 alg2 str = property $ liftBool (alg1 str == alg2 str)

forwardCompatible :: (Char8Find str, Eq str) => (str -> Either NonAsciiChar str) -> str -> Property
forwardCompatible = equivalProperty newImpl


type NonAsciiChar = ASCII.NonAsciiChar


oldImplS = oldEncodeImpl L.partition L.head L.null
oldImplT = oldEncodeImpl T.partition T.head T.null
oldImplTL = oldEncodeImpl TL.partition TL.head TL.null
oldImplB = oldEncodeImpl (\p -> B8.filter p &&& B8.filter (not . p)) B8.head B8.null
oldImplBL = oldEncodeImpl (\p -> BL8.filter p &&& BL8.filter (not . p)) BL8.head BL8.null


newImpl :: Char8Find str => str -> Either NonAsciiChar str
newImpl = ASCII.encImpl

oldEncodeImpl :: 
   ((Char -> Bool) -> a -> (a, a))
   -> (a -> Char)
   -> (a -> Bool)
   -> a
   -> Either NonAsciiChar a
oldEncodeImpl partitionf headf nullf t = 
                 let (tascii, nonascii) = partitionf isAscii t 
                 in if nullf nonascii 
                    then Right tascii
                    else Left . ASCII.NonAsciiChar $ headf nonascii 

spec :: Spec 
spec = 
    describe "ASCII backward compapatibilty check" $ do
       it "String" $ property $ forwardCompatible oldImplS 
       it "Text" $ property $ forwardCompatible oldImplT
       it "Lazy.Text" $ property $ forwardCompatible oldImplTL
       it "ByteString" $ property $ forwardCompatible oldImplB
       it "Lazy.ByteString" $ property $ forwardCompatible oldImplBL

runSpec :: IO ()
runSpec = hspec spec       