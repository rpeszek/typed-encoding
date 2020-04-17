# typed-encoding
Type level transformations to make programming Strings better

## Motivation
I recently had some bad experience with using ByteString and Text with `Base64` and
`quoted-printable` encoding.
I was troubleshooting text being double encoded or not encoded at all.   
Issues like these do not manifest themselves with run-time errors.  The issues are discovered by visually inspecting generated documents.
What if the encodings were visible at the type level...

```Haskell
myData :: Enc '["B64"] ByteString
```

## About this library
... but this approach seems to me to be much more...

```Haskell
-- upper cased text encoded as base64
example :: Enc '["B64", "UPPER"] () T.Text
example = encodeAll . toEncoding () $ "some text goes here"
```
It becomes a declarative style of applying string transformations.

Transformations can be
   - used with parameters.
   - applied partially
   - undone or undone partially (id encoding is reversible)
   - can be effectful

The approach seems like a different take on programming. 
A form of type directed program synthesis - programs are a boilerplate and the game is played at the type level. 

Here are some [examples](src/Examples.hs).

## Dependencies

Currently it uses
   - `base64-bytestring` because it was my driving example
   - `aeson` because it makes sense to define consistent JSON instances for `Enc`
   - I will try to separate bigger deps like servant into separate libs if there is interest

