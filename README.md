# typed-encoding
Type level encodings to make programming Strings better

## Motivation
I recently had some bad experience with using ByteString and Text with Base64 and
quoted-printable encodings.
I was dealing with text being double encoded or not encoded at all.   
The pain point is that you do not learn until very and, not until you try to open the end result and everything looks corrupted, or it does not unless it has chars outside UTF-7 range.

So I wanted the encodings to be visible at type level...

## Other stuff
But this approach seems to me like much more...