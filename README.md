# typed-encoding
Type level annotations, string transformations, and other goodies that make programming strings safer.

## Motivation
I have recently spent a lot of time troubleshooting various `Base64`, `quoted-printable`, and `UTF-8` encoding issues.  
I decided to write a library that will help avoiding issues like these.

This library allows to specify and work with types like

```Haskell
-- some data encoded in base 64
mydata :: Enc '["enc-B64"] ByteString

-- some text (utf8) data encoded in base 64 
myData :: Enc '["enc-B64", "r-UTF8"] ByteString
```

It allows to define precise string content annotations like:

```Haskell
ipaddr :: Enc '["r-IpV4"] Text
```

and provides ways for 

- encoding
- decoding
- recreation (encoding validation)
- type conversions
- converting types to encoded strings
- typesafe conversion of encoded strings to types

... but this approach seems to be a bit more...

```Haskell
-- upper cased text encoded as base64
example :: Enc '["enc-B64", "do-UPPER"] () T.Text
example = encodeAll . toEncoding () $ "some text goes here"
```

It becomes a type directed, declarative approach to string transformations.

Transformations can be

- used with parameters
- applied or undone partially (if encoding is reversible)

One of more intersting uses of this library are encoding restrictions.   
(Arbitrary) bounded alpha-numeric (`r-ban`) restrictions 
and a simple annotation boolean algebra are both provided.

```Haskell
phone :: Enc '["r-ban:999-999-9999"] () T.Text
phone = ...

-- simple boolean algebra:
phone' :: Enc '["boolOr:(r-ban:999-999-9999)(r-ban:(999) 999-9999)"] () T.Text
phone' = ...
```


## Examples 

Here are some code examples:

- [Overview](src/Examples/TypedEncoding/Overview.hs)
- [Conversions between encodings](src/Examples/TypedEncoding/Conversions.hs)
- [Adding a new encoding, error handling](src/Examples/TypedEncoding/DiySignEncoding.hs)
- [To and from string conversions](src/Examples/TypedEncoding/ToEncString.hs)
- [Unsafe - working inside encodings](src/Examples/TypedEncoding/Unsafe.hs)
 

## Hackage

https://hackage.haskell.org/package/typed-encoding


## Other encoding packages

My approach will be to write specific encodings (e.g. _HTTP_) or wrap encodings from other packages using separate "bridge" projects.

Currently `typed-encoding` depends on

- `base64-bytestring` because it was my driving example, this is likely to move out to a separate bridge project at some point. 

Bridge work:

- [typed-encoding-encoding](https://github.com/rpeszek/typed-encoding-encoding) bridges [encoding](https://github.com/dmwit/encoding) package

## Plans, some TODOs

- lensifying conversions 
- better implementation type safety

## Tested with
- stack (1.9.3) lts-14.27 (ghc-8.6.5)
- needs ghc >= 8.2.2, base >=4.10 for GHC.TypeLits support

## Known issues
- running test suite: cabal has problems with doctest, use stack  
   https://github.com/haskell/cabal/issues/6087   
