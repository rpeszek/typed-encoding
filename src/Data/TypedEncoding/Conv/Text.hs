{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-} -- removes need to annotate kinds as [Symbol]

-- | Text encoding combinators specific to 'T.Text'
-- @since 0.2.2.0
module Data.TypedEncoding.Conv.Text where

import qualified Data.Text as T
import           Data.TypedEncoding.Instances.Support


-- $setup
-- >>> :set -XDataKinds -XTypeFamilies -XTypeApplications


-- | This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @Text@. 
pack :: Enc xs c String -> Enc xs c T.Text
pack = unsafeChangePayload T.pack

-- | This assumes that each of the encodings in @xs@ work work equivalently in @String@ and @Text@. 
unpack :: Enc xs c T.Text -> Enc xs c String
unpack = unsafeChangePayload T.unpack 

-- | 
-- Text is automatically @"r-UTF8"@ encoded
-- 
-- Adding @"r-UTF8"@ annotation simply adds type level interpretion requirement that 'T.Text' is treated
-- as /UTF8/. The internals of 'T.Text' (currently /UTF-16/) are not relevant and @utf8Promote@ is implemented
-- as 'id'.  This is not the same as encoding @Word8@ layouts into 'Char'-s.
-- This, in /typed-encoding/ terminology, would be @"enc-UTF8"@, not @"r-UTF8".
--
-- >>> displ $ utf8Promote $ toEncoding () ("text" :: T.Text)
-- "Enc '[r-UTF8] () (Text text)"
utf8Promote :: Enc xs c T.Text -> Enc (Snoc xs "r-UTF8") c T.Text
utf8Promote = withUnsafeCoerce id

-- | 
-- For 'T.Text' @"r-UTF8"@ is redundant
--
-- >>> displ . utf8Demote $ (unsafeSetPayload () "Hello" :: Enc '["r-UTF8"] () T.Text)
-- "Enc '[] () (Text Hello)"
utf8Demote :: (UnSnoc xs ~ '(,) ys "r-UTF8") => Enc xs c T.Text -> Enc ys c T.Text
utf8Demote = withUnsafeCoerce id