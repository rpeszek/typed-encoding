

{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Checks for use of IsStringR which will be deprecated
--
-- it is used for "r-Word8-decimal" 
--
-- * FromEncString 
-- * EncodeF
-- * DecodeF
-- * RecreateF

module Test.Bc.IsStringRSpec where

import           Test.Hspec

import           Data.Word
import           Data.Either
import           Data.TypedEncoding
import           Data.TypedEncoding.Internal.Class.IsStringR 
import           Data.TypedEncoding.Instances.Restriction.Common ()
import           Data.TypedEncoding.Instances.ToEncString.Common ()

newtype MyStr = MyStr String deriving (Eq, Show)

instance IsStringR MyStr
  where toString (MyStr str) = str

tstWord8 = MyStr "123"
tstNotWord8 = MyStr "hello"
tstNotWord8' = MyStr "256"

tstWord8Enc :: Enc '["r-Word8-decimal"] () MyStr
tstWord8Enc = unsafeSetPayload () tstWord8

tstWord8EncErr :: Enc '["r-Word8-decimal"] () MyStr
tstWord8EncErr = unsafeSetPayload () tstNotWord8

fromEncStringTest :: Word8
fromEncStringTest = fromEncString tstWord8Enc

fromEncStringTestErr :: Either UnexpectedDecodeEx Word8
fromEncStringTestErr = fromEncStringF tstWord8EncErr


spec :: Spec    
spec = 
    describe "IsStringR constr works" $ do
        it "Word8 fromEncString works" $ 
           fromEncStringTest `shouldBe` 123 
        it "Word8 fromEncString err works" $   
           fromEncStringTestErr `shouldSatisfy` isLeft
        it "EncodeF works" $
          (encFAll @'["r-Word8-decimal"] @(Either EncodeEx) . toEncoding () $ tstWord8) `shouldSatisfy` isRight   
        it "EncodeF err works" $
          (encFAll @'["r-Word8-decimal"] @(Either EncodeEx). toEncoding () $ tstNotWord8') `shouldSatisfy` isLeft
        it "RecreateF works" $
          (recreateFAll @(Either RecreateEx) @'["r-Word8-decimal"] . toEncoding () $ tstWord8) `shouldSatisfy` isRight   
        it "RecreateF err works" $
          (recreateFAll @(Either RecreateEx) @'["r-Word8-decimal"] . toEncoding () $ tstNotWord8) `shouldSatisfy` isLeft
        it "DecodeF works" $
          (fromEncoding . decAll $ tstWord8Enc) `shouldBe` tstWord8   
   
runSpec :: IO ()
runSpec = hspec spec           