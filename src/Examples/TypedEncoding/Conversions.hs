{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- | Examples or moving between type annotated encodings
--
-- Haskell programs typically make these imports to do String, ByteString, and Text conversions:
--
-- @
-- import qualified Data.Text as T (pack, unpack)
-- import qualified Data.ByteString.Char8 as B8 (pack, unpack)
-- import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
-- @
--
-- or corresponding @Lazy@ imports (not shown).
--
-- Enc-specific equivalents can be found in:
--
-- @
-- import qualified Data.TypedEncoding.Conv.Text as EncT (pack, unpack)
-- import qualified Data.TypedEncoding.Conv.ByteString.Char8 as EncB8 (pack, unpack)
-- import           Data.TypedEncoding.Conv.Text.Encoding (decodeUtf8, encodeUtf8)
-- @    
--
-- Conversions aim at providing type safety when moving between encoded string-like types.
--
-- __The assumption__ made by `typed-encoding` is that encodings work in equivalent way independently of the payload type.
-- For example, if the following instances exist:
--
-- @
-- EncodeF SomeErr (Enc xs () String) (Enc ("enc-B64" ': xs) () String)    
-- EncodeF SomeErr (Enc xs () Text) (Enc ("enc-B64" ': xs) () Text)    
-- @
-- 
-- Then @typed-encoding@ expects @pack@ @encodeF@ to commute:
-- 
-- @
--  str     -- EncT.pack -->   txt
--   |                          |
--  encodeF                  encodeF
--   |                          | 
--   v                          v
--  estr -- fmap EncT.pack --> etxt
-- @
--
-- (@unpack@ and $decode$ are expected to satisfy similar diagrams, not shown)
--
-- Basically, it should not matter which type we run the encoding on (other than performance cost).
--
--
-- This module also discusses concepts of __Superset__ (for @"r-"@ encodings), __leniency__, and __flattening__. 
module Examples.TypedEncoding.Conversions where

import           Data.TypedEncoding
import           Data.TypedEncoding.Instances.Enc.Base64 () 
import           Data.TypedEncoding.Instances.Restriction.ASCII ()
import           Data.TypedEncoding.Instances.Restriction.UTF8 ()

import qualified Data.TypedEncoding.Conv.Text as EncT 
import qualified Data.TypedEncoding.Conv.Text.Encoding as EncTe (decodeUtf8)

import qualified Data.Text as T
import qualified Data.ByteString as B
import           GHC.TypeLits

import qualified Data.TypedEncoding.Conv.ByteString.Char8 as EncB8
import           Data.TypedEncoding.Combinators.Restriction.BoundedAlphaNums ()

-- $setup
-- >>> :set -XDataKinds -XMultiParamTypeClasses -XKindSignatures -XFlexibleInstances -XFlexibleContexts -XOverloadedStrings -XTypeApplications -XScopedTypeVariables
-- >>> import qualified Data.TypedEncoding.Instances.Enc.Base64 as EnB64 (acceptLenientS)
-- >>> import qualified Data.TypedEncoding.Conv.Text as EncT (pack, utf8Promote, utf8Demote)
-- >>> import qualified Data.TypedEncoding.Conv.ByteString.Char8 as EncB8 (pack, unpack)
-- >>> import qualified Data.TypedEncoding.Conv.Text.Encoding as EncTe (decodeUtf8, encodeUtf8)
-- >>> import           Data.Proxy
--
-- This module contains some ghci friendly values to play with.
--
-- Each value is documented in a doctest style by including an equivalent ghci ready expression.
-- These documents generate a test suite for this library as well.


-- * Moving between Text and ByteString

eHelloAsciiB :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString)
eHelloAsciiB = encodeFAll . toEncoding () $ "HeLlo world" 
-- ^ Example value to play with
--
-- >>>  encodeFAll . toEncoding () $ "HeLlo world" :: Either EncodeEx (Enc '["r-ASCII"] () B.ByteString) 
-- Right (MkEnc Proxy () "HeLlo world")

Right helloAsciiB = eHelloAsciiB
-- ^ above with either removed

helloAsciiT :: Enc '["r-ASCII"] () T.Text
helloAsciiT = EncTe.decodeUtf8 helloAsciiB
-- ^ 
-- We use a tween function to the popular 'Data.Text.Encoding.decodeUtf8' 
-- from the @test@ package.
--
-- Notice the encoding annotation is preserved.
--
-- >>> displ $ EncTe.decodeUtf8 helloAsciiB
-- "MkEnc '[r-ASCII] () (Text HeLlo world)"


-- * pack and unpack

helloZero :: Enc ('[] :: [Symbol]) () String
helloZero = toEncoding () "Hello"
-- ^ Consider 0-encoding of a 'String',  to move it to @Enc '[] () String@ one could try:
--
-- >>> displ . EncT.pack $ helloZero
-- "MkEnc '[] () (Text Hello)"
--
-- this works, but:
-- 
-- >>> EncB8.pack helloZero
-- ...
-- ... error: 
-- ... Empty Symbol list not allowed
-- ...
--
-- this does not compile.  And it should not. @pack@ from "Data.ByteString.Char8" is error prone.
-- It is not an injection as it only considers first 7 bits of information from each 'Char'.  
-- I doubt that there are any code examples of its intentional use on a String that is not ASCII. 
-- 
-- @EncB8.pack@ will not compile unless the encoding is ASCII restricted, this works:
-- 
-- >>> fmap (displ . EncB8.pack) . encodeFAll @(Either EncodeEx) @'["r-ASCII"] $ helloZero
-- Right "MkEnc '[r-ASCII] () (ByteString Hello)"
--
-- And the result is a @ByteString@ with bonus annotation describing its content.


helloRestricted :: Either EncodeEx (Enc '["r-ban:zzzzz"] () B.ByteString)
helloRestricted = fmap EncB8.pack . runEncoder @'["r-ban"] encodings $ toEncoding () "Hello"
-- ^ more interstingly @EncB8.pack@ works fine on "r-" encodings that are subsets of "r-ASCII"
-- this example @"r-ban:zzzzz"@ restricts to 5 alapha-numeric charters all < 'z'
-- 
-- >>> displ <$> helloRestricted
-- Right "MkEnc '[r-ban:zzzzz] () (ByteString Hello)"
--
-- Adding @"r-ASCII"@ annotation on this ByteString would have been redundant since @"r-ban:zzzzz"@ is more
-- restrictive (see Supersets below).
--
-- @unpack@, as expected will put us back in a String keeping the annotation
--
-- >>> fmap (displ . EncB8.unpack) helloRestricted
-- Right "MkEnc '[r-ban:zzzzz] () Hello"
-- 


-- * Supersets

helloUtf8B :: Enc '["r-UTF8"] () B.ByteString
helloUtf8B = injectInto helloAsciiB
-- ^ To get UTF8 annotation, instead of doing this: 
--
-- >>> encodeFAll . toEncoding () $ "HeLlo world" :: Either EncodeEx (Enc '["r-UTF8"] () B.ByteString)
-- Right (MkEnc Proxy () "HeLlo world")
-- 
-- We should be able to convert the ASCII version we already have.
--
-- This is done using 'Superset' typeclass.
--
-- @injectInto@ method accepts proxy to specify superset to use.
--
-- >>> displ $ injectInto @ "r-UTF8" helloAsciiB
-- "MkEnc '[r-UTF8] () (ByteString HeLlo world)"
--
-- Superset is intended for @"r-"@ annotations only, should not be used
-- with general encodings like @"enc-B64"@, it assumes that decoding in the superset
-- can replace the decoding from injected subset.



-- * More complex rules

helloUtf8B64B :: Enc '["enc-B64", "r-UTF8"] () B.ByteString
helloUtf8B64B = encodePart @'["enc-B64"] helloUtf8B 
-- ^ We put Base64 on a ByteString which adheres to UTF8 layout
--
-- >>> displ $ encodePart_ (Proxy :: Proxy '["enc-B64"]) helloUtf8B
-- "MkEnc '[enc-B64,r-UTF8] () (ByteString SGVMbG8gd29ybGQ=)"

helloUtf8B64T :: Enc '["enc-B64"] () T.Text
helloUtf8B64T = EncT.utf8Demote . EncTe.decodeUtf8 $ helloUtf8B64B  
-- ^ .. and copy it over to Text.
--
-- >>> displ $ EncTe.decodeUtf8 helloUtf8B64B
-- "MkEnc '[enc-B64,r-UTF8] () (Text SGVMbG8gd29ybGQ=)"
--
-- but UTF8 would be redundant in Text so the "r-UTF8" can be dropped:
--
-- >>> displ . EncT.utf8Demote . EncTe.decodeUtf8 $ helloUtf8B64B
-- "MkEnc '[enc-B64] () (Text SGVMbG8gd29ybGQ=)"
--
-- Conversely moving back to ByteString we need to recover the annotation
-- 
-- >>> :t EncTe.encodeUtf8 helloUtf8B64T
-- ...
-- ... Couldn't match type ‘IsSupersetOpen
-- ... "r-UTF8" "enc-B64" ...
-- ...
--
-- This is not allowed! We need to add the redundant "r-UTF8" back:
--
-- >>> displ .  EncTe.encodeUtf8 . EncT.utf8Promote $ helloUtf8B64T
-- "MkEnc '[enc-B64,r-UTF8] () (ByteString SGVMbG8gd29ybGQ=)"
--
-- To achieve type safety, our @encodeUtf8@ and @decodeUtf8@ require "r-UTF8" annotation. 
-- But since @Text@ values can always emit @UTF8@ layout, we can simply add and remove
-- these annotations on @Text@ encodings.  This approach gives us type level safety over UTF8 encoding/decoding errors.

notTextB :: Enc '["enc-B64"] () B.ByteString
notTextB = encodeAll . toEncoding () $ "\195\177"
-- ^ 'notTextB' a binary, one that does not even represent a valid UTF8.
-- 
-- >>> encodeAll . toEncoding () $ "\195\177" :: Enc '["enc-B64"] () B.ByteString
-- MkEnc Proxy () "w7E="
--
-- Decoding it to Text is prevented by the compiler
--
-- >>> :t EncTe.decodeUtf8 notTextB
-- ...
-- ... error:
-- ... Couldn't match type ...
-- ... "r-UTF8" "enc-B64" ...
-- ...
--
-- This is good because having the payload inside of @Enc '["enc-B64"] () Text@ would allow us
-- to try to decode it to Text (causing runtime errors).
-- 
-- We can move it to Text but to do that we will need to forget the "enc-B64" annotation.
-- This can be done, for example, using flattening (see below).


-- * Lenient recovery

lenientSomething :: Enc '["enc-B64-len"] () B.ByteString
lenientSomething = recreateAll . toEncoding () $ "abc==CB"
-- ^ 
-- >>> recreateAll . toEncoding () $ "abc==CB" :: Enc '["enc-B64-len"] () B.ByteString
-- MkEnc Proxy () "abc==CB"
--
-- The rest of Haskell does lenient decoding, type safety allows this library to use it for recovery.
-- lenient algorithms are not partial and automatically fix invalid input:
--
-- >>> recreateFAll . toEncoding () $ "abc==CB" :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
-- Left (RecreateEx "enc-B64" ("invalid padding"))
--
-- This library allows to recover to "enc-B64-len" which is different than "enc-B64"
--
-- 'EnB64.acceptLenientS' allows to convert "enc-B64-len" to "enc-B64"
--
-- >>> displ $ EnB64.acceptLenientS lenientSomething
-- "MkEnc '[enc-B64] () (ByteString abc=)"
--
-- This is now properly encoded data
--
-- >>> recreateFAll . toEncoding () $ "abc=" :: Either RecreateEx (Enc '["enc-B64"] () B.ByteString)
-- Right (MkEnc Proxy () "abc=")
--
-- Except the content could be surprising
--
-- >>> decodeAll $ EnB64.acceptLenientS lenientSomething
-- MkEnc Proxy () "i\183"


-- * Flattening

b64IsAscii :: Enc '["r-ASCII"] () B.ByteString
b64IsAscii = flattenAs helloUtf8B64B
-- ^ Base 64 encodes binary data as ASCII text. 
-- thus, we should be able to treat "enc-B64" as "r-ASCII" losing some information.
-- this is done using 'FlattenAs' type class
--
-- >>> :t flattenAs @ "r-ASCII" helloUtf8B64B
-- flattenAs @ "r-ASCII" helloUtf8B64B
-- ... :: Enc '["r-ASCII"] () B.ByteString
