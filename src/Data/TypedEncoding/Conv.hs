

-- |
-- Conversion combinator module structure is similar to one found in /text/ and /bytestring/ packages
-- And can be found nested under this module:
--
-- * "Data.TypedEncoding.Conv.Text"
-- * "Data.TypedEncoding.Conv.Text.Encoding"
-- * "Data.TypedEncoding.Conv.Text.Lazy"    
-- * "Data.TypedEncoding.Conv.Text.Lazy.Encoding"
-- * "Data.TypedEncoding.Conv.ByteString.Char8"
-- * "Data.TypedEncoding.Conv.ByteString.Lazy.Char8"
--
-- Two goals of these conversions are:
--
-- * provide added type safety for string conversions
-- * provide a way to easily convert encoded data directly between /text/ and /bytestring/ types.
--
--
-- == Type Safety
--
-- Haskell has 3 (5 counting lazy versions) popular string types, unfortunately they are all quite different.
--
-- Consider these 4 popular conversion functions:
--
-- @
-- import qualified Data.Text as T
-- import qualified Data.ByteString.Char8 as B8
-- import qualified Data.Text.Encoding as TE
--
-- T.pack        :: String -> T.Text 
-- T.unpack      :: T.Text -> String
-- B8.pack       :: String -> B8.ByteString
-- B8.unpack     :: B8.ByteString -> String
-- TE.decodeUtf8 :: B8.ByteString -> T.Text 
-- TE.encodeUtf8 :: T.Text -> B8.ByteString
-- @
--
-- They come in pairs but are not reversible.  
--
-- Going from A to C depends on B, none of the following 4 diagrams commutes:
--
-- @
--  String -> B8.pack ->   ByteString
--   ^                    ^        |
--   |                    | TE.decodeUtf8
--  id                    |        |
--   |               TE.encodeUtf8 |
--   v                    |        v
--  String -> T.pack ->      Text
-- @
--
-- @
--  String <- B8.unpack <- ByteString
--   ^                      ^      |
--   |                      | TE.decodeUtf8
--  id                      |      |
--   |               TE.encodeUtf8 |
--   v                      |      v
--  String <- T.unpack  <-    Text
-- @
--
-- All of this can lead to bugs that are hard to find and hard to troubleshoot.
--
-- /typed-encoding/ provides more precise types so that all of this goes away.
-- 
-- Here are the type signatures simplified to one single encoding annotation:
--
-- @
-- import qualified Data.TypedEncoding.Conv.Text as ET
-- import qualified Data.TypedEncoding.Conv.ByteString.Char8 as EB8
-- import qualified Data.TypedEncoding.Conv.Text.Encoding as ETE
--
-- ET.pack        :: (Superset "r-UNICODE.D76" r) => Enc '[r] c String -> Enc '[r] c T.Text 
-- ET.unpack      :: (Superset "r-UNICODE.D76" r) => Enc '[r] c T.Text -> Enc '[r] c String
-- EB8.pack       :: (Superset "r-CHAR8" r)       => Enc '[r] c String -> Enc '[r] c B8.ByteString
-- EB8.unpack     :: (Superset "r-CHAR8" r)       => Enc '[r] c B8.ByteString -> Enc '[r] c String
-- ETE.decodeUtf8 :: (Superset "r-UTF8" r)        => Enc '[r] c B8.ByteString -> Enc '[r] c T.Text 
-- ETE.encodeUtf8 :: (Superset "r-UTF8" r)        => Enc '[r] c T.Text -> Enc '[r] c B8.ByteString  
-- @
--
-- @"r-UNICODE.D76"@ and @"r-UTF8"@ is considered redundant for @T.Text@ and can be added or dropped as needed.
-- 
-- Corresponding pairs reverse, this should be clear since the types are restricted to what @T.Text@ can store or to 
-- how @B8.Char@ works.
--
-- Now consider any of the above diagrams, for instance, compare
-- 
-- @
-- ETE.encodeUtf8 . ET.pack :: (Superset "r-UNICODE.D76" r, Superset "r-UTF8" r) => Enc '[r] c String -> Enc '[r] c B8.ByteString 
-- -- and 
-- EB8.pack :: (Superset "r-CHAR8" r) => Enc '[r] c String -> Enc '[r] c B8.ByteString
-- @  
--
-- What is the set of common values allowing us to use any of these 2 options?
--
-- "r-UNICODE.D76" is not important here (it removes a range of Unicode values way above @\'\255\'@), what is the intersection of /UTF8/ and /CHAR8/ code point space?
--
-- There are many character set encodings that utilize one byte (/CHAR8/) and /UTF8/ is different from all of them
-- but it backward compatible only within the /ASCII/ range of chars @ < 127@.  So the intersection should be /ASCII/, let us check that:
--
-- @
-- ghci> :t ETE.encodeUtf8 . ET.pack @'["r-ASCII"]
-- EncTe.encodeUtf8 . EncT.pack @'["r-ASCII"]
--  :: Enc [Symbol] ((':) Symbol "r-ASCII" ('[] Symbol)) c String
--     -> Enc [Symbol] ((':) Symbol "r-ASCII" ('[] Symbol)) c B.ByteString
--
-- ghci> :t EncB8.pack @'["r-ASCII"]
--  :: Enc [Symbol] ((':) Symbol "r-ASCII" ('[] Symbol)) c String
--    -> Enc [Symbol] ((':) Symbol "r-ASCII" ('[] Symbol)) c B.ByteString
-- @  
--
-- They both accept that common denominator.  
-- Now we could run a property test but it is clear that by the design these will match!
--    
-- Note, there is no @Superset "r-UNICODE.D76" "r-CHAR8"@ mapping, "r-CHAR8" supersets any
-- 8-bit encoding like /ISO/IEC 8859/ family of encodings.  This is by design even if structurally such 
-- definition would made sense.
--
-- This choice effectively prevents anything classified under @"r-CHAR8"@
-- to end up as a visible encoding annotation in @Text@ (since that would made little sense as @Text@ is /UTF/ encoded).  
-- This is just one example of added type level security that /type-encoding/ provides.
--
-- Currently, "r-CHAR8" is intended as upper bound on "r-" encodings only. There is no way
-- to encode to it using provided encoding mechanisms (except for /unsafe/ options).
-- Effectively the types
-- 
-- @
-- Enc "r-CHAR8" c str
-- @
--
-- can be viewed as uninhabited. 
--
-- However, @Char@ is often used instead of @Word8@
-- for low level @ByteString@ programming. This is supported with the @"r-ByteRep"@ annotation.
--
-- @
-- Enc "r-ByteRep" c str
-- @
--
-- this one can be used as @Superset "r-CHAR8" "r-ByteRep"@! 
-- That allows for @EncB8@ conversions to work on such data.
-- However, there is no @Superset "r-UNICODE.D76" "r-ByteRep"@ so these cannot be converted to @Text@, which is 
-- exactly what is intended. 
--
--
-- == @Enc@ conversions 
-- 
-- Consider defining a conversion function @:: Enc xs c str1 -> f (Enc xs c str2)@.
--
-- One challenge is how do we know that @xs@ is a valid encoding stack also for @str2@?
-- Should we constrain that?      
--
-- This is made even more difficult because this library uses (has to) orphan instances.
--
-- The other challenge is how to ensure that, if the destination is partially or fully decoded, then
-- it will decode without errors and the decoding will be meaningful.
--
-- Current version does not impose any instance constraint about existence of the stack for @str2@. 
-- It is possible to not have one, in that case @decodeAll@ combinators will not be available.
--
-- This is still useful as the payload could be safely extracted, to save to the database or do other things with it.
--
-- Future versions of /typed-encoding/ may provide ways to ensure validity of the encoding stack for @str2@.


module Data.TypedEncoding.Conv where