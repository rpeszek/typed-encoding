# typed-encoding
Type level annotations, string transformations, and other goodies that make programming strings safer.

## Motivation
I have recently spent a lot of time troubleshooting various `Base64`, `quoted-printable`, and `UTF-8` encoding issues.  
I decided to write a library that will help avoiding issues like these.

This library allows to specify and work with types like

```Haskell
-- some data encoded in base 64
mydata :: Enc '["enc-B64"] c ByteString

-- some text (utf8) data encoded in base 64 
myData :: Enc '["enc-B64", "r-UTF8"] c ByteString
```

It allows to define precise string content annotations like:

```Haskell
ipaddr :: Enc '["r-IpV4"] c Text
```

and provides ways for 

- encoding
- decoding
- recreation (encoding validation)
- type conversions
- converting types to encoded strings
- typesafe conversion of encoded strings to types

Partial and dangerous things like `decodeUtf8` are no longer dangerous, 
`ByteString` `Text` conversions become fully reversible.  Life is good!

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

One of more interesting uses of this library are encoding restrictions included in this library.   
Example are (arbitrary) bounded alpha-numeric (`"r-ban"`) restrictions.

```Haskell
-- allow only properly formatted phone numbers

type PhoneSymbol = "r-ban:999-999-9999"
phone :: Enc '[PhoneSymbol] () T.Text
phone = ... 
```

The author often uses _typed_encoding_ with _Servant_ (`HttpApiData` instances are not included), e.g.:

```Haskell
type LookupByPhone = 
  "customer"
  :> "byphone"
  :> Capture "phone" (Enc '[PhoneSymbol] () T.Text)
  :> Get '[JSON] ([Customer])
```

or to get type safety over text document using Unix vs Windows line breaks!


## Goals and limitations

The main goal is to provide improved type safety for programs that use string encodings and 
transformations.  _Not to provide encoding implementation type safety_. 
_Encoding and string manipulation libraries are typically well established and tested, type safety is really needed at the usage site, not at the implementation site_.

This library approach is to fight issues with (value level) strings using (type level) strings. Using `Symbol`-s effectively forces us to play the orphan instances game.   
One of the long term goals is for this library to provide combinator alternatives to typeclass polymorphism so that the orphan instances are more of a convenience and not the necessity.  


## Examples 

Here are some code examples:

- [Overview](src/Examples/TypedEncoding/Overview.hs)
- [Conversions between encodings](src/Examples/TypedEncoding/Conversions.hs)
- [DIY encoding, error handling](src/Examples/TypedEncoding/Instances/DiySignEncoding.hs)
- [To and from string conversions](src/Examples/TypedEncoding/ToEncString.hs)
- [Unsafe - working inside encodings](src/Examples/TypedEncoding/Unsafe.hs)
 

## Hackage

https://hackage.haskell.org/package/typed-encoding

## Other encoding packages

My approach will be to write specific encodings (e.g. _HTTP_) or wrap encodings from other packages using separate "bridge" projects.

Currently /typed-encoding/ depends on

- /base64-bytestring/ because it was my driving example, this is likely to move out to a separate bridge project at some point. 

Bridge work:

- [typed-encoding-encoding](https://github.com/rpeszek/typed-encoding-encoding) bridges [encoding](https://github.com/dmwit/encoding) package 


## Tested with

- stack (1.9.3) lts-14.27 (ghc-8.6.5)
- needs ghc >= 8.2.2, base >=4.10 for GHC.TypeLits support

## Known issues

- running test suite: cabal has problems with doctest, use stack  
   https://github.com/haskell/cabal/issues/6087   
