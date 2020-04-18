# typed-encoding
Type level annotations that make programming Strings better

## Motivation
I recently had some bad experience with using ByteString and Text with `Base64` and
`quoted-printable` encoding.
I was troubleshooting text being double encoded or not encoded at all. The decoded text ended up being converted to `Data.Text` but it was a binary image...   
Issues like double encoding do not manifest themselves with run-time errors.  The issues are discovered by visually inspecting generated documents.
What if the encodings were visible at the type level...

```Haskell
myData :: Enc '["enc-B64"] ByteString
```
that would be really nice! But what would be even nicer if we could do something like
```Haskell
myData :: Enc '["enc-B64", "r-ASCII"] ByteString
```
So, after decoding Base64 I would know that what is left is not some jpeg image...

## About this library
... but this approach seems to me to be much more...

```Haskell
-- upper cased text encoded as base64
example :: Enc '["enc-B64", "do-UPPER"] () T.Text
example = encodeAll . toEncoding () $ "some text goes here"
```
It becomes a declarative style of applying string transformations.

Transformations can be
   - used with parameters.
   - applied or undone partially (if encoding is reversible)
   - effectful

And these annotations are not only for transformations.  Annotations can be used
to restrict type to smaller set of values (`r-ASCII`, `r-UTF8`), they 
can be converted to other annotations (eg. `r-ASCII` is subset of `r-UTF8`) ...

The approach seems like a different take on programming: 
a form of type directed program synthesis - programs become a boilerplate and the game is played at the type level. 

Here are some examples:
   - [Overview Code Examples](src/Examples_Intro.hs)
   - TODO [Conversions between encodings](src/Examples_Conversions.hs)
   - TODO [adding new conversion example](src/Examples_Conversions.hs)

## Dependencies

Currently it uses
   - `base64-bytestring` because it was my driving example
   - `aeson` because it makes sense to define consistent JSON instances for `Enc`
   - I will try to separate other deps like `servant` into separate libs if there is interest

