{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- | Verified backward compatibility of ASCII encoder changes in v0.3
module Test.SupersetSpec where


import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()
import           Data.TypedEncoding.Instances.Restriction.D76 ()
import           Data.TypedEncoding.Instances.Restriction.ByteRep ()
import           Data.TypedEncoding.Instances.Enc.Base64 ()
import qualified Data.TypedEncoding.Instances.Restriction.CHAR8 as CHAR8

import           Data.TypedEncoding -- .Common.Class.Superset

import           Test.QuickCheck.Instances.ByteString ()
import qualified Data.ByteString as B
import           Test.QuickCheck
-- import           Test.QuickCheck.Property
import           Test.Hspec


spec :: Spec 
spec =  
    describe "Superset tests" $ do
        describe "ByteString based" $ do
            it "r-UTF8 > r-ASCII" $ property $ propSuperset_ @"r-UTF8" @"r-ASCII" @B.ByteString encoding encoding 
            it "r-CHAR8 > r-ASCII" $ property $ propSuperset_ @"r-CHAR8" @"r-ASCII" @B.ByteString CHAR8.testEncCHAR8 encoding
            it "r-CHAR8 > r-ByteRep" $ property $ propSuperset_ @"r-CHAR8" @"r-ByteRep" @B.ByteString CHAR8.testEncCHAR8 encoding
        describe "String based" $ 
            it "r-UNICODE.D76 > r-ASCII" $ property $ propSuperset_ @"r-UNICODE.D76" @"r-ASCII" @String encoding encoding
        describe "EncodingSuperset test" $
            it "enc-B64 < r-ASCII" $ property $ propEncodesInto_ @"enc-B64" @"r-B64" @B.ByteString encoding encoding
runSpec :: IO ()
runSpec = hspec spec   