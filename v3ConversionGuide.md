

## Encoding implementation:

```Haskell
-- v0.2:
instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    encodeF = implEncodeP B64.encode 

-- v0.3:

instance Applicative f => Encode f "enc-B64" "enc-B64" c B.ByteString where
    encoding = encB64B

encB64B :: Applicative f => Encoding f "enc-B64" "enc-B64" c B.ByteString
encB64B = mkEncoding (implEncodeP B64.encode)


```

TODO
```
-- v0.2:
instance Encodings (Either EncodeEx) xs grps c B.ByteString => Encodings (Either EncodeEx) ("enc-B64" ': xs) ("enc-B64" ': grps) c B.ByteString where
    encodings = encodeFEncoder @(Either EncodeEx) @"enc-B64" @"enc-B64"

-- v0.3:
instance WhichEncoder (Either EncodeEx) xs grps c B.ByteString => WhichEncoder (Either EncodeEx) ("enc-B64" ': xs) ("enc-B64" ': grps) c B.ByteString where
    encoder = encodeFEncoder @(Either EncodeEx) @"enc-B64" @"enc-B64"
```

encodeF -> encF
encodeAll -> encAll 
encodeFAll -> encFAll (forall order changed, first to type variables flipped to nms f c str from f nms)

```
encodeFAll
  :: EncodeFAll f xs c str =>
     Enc [k] ('[] k) c str
     -> f (Enc [Symbol] xs c str)

encFAll
  :: (Monad f, EncodeAll f nms nms c str) =>
     Enc
       [Symbol]
       ('[] Symbol)
       c
       str
     -> f (Enc [Symbol] nms c str)     
```

### Decoding

```
instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implDecodeF (asUnexpected @"enc-B64" . B64.decode) 


instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c B.ByteString where
    decoding = decB64B

decB64B :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c B.ByteString
decB64B = mkDecoding $ implDecodeF (asUnexpected @"enc-B64" . B64.decode)

```

decodeF -> decF
decodeAll -> decAll 
decodeFAll -> decFAll (forall order changed, first to type variables flipped to nms f c str from f nms)


### Validation
```
instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implDecodeF (asUnexpected @"enc-B64" . B64.decode) 


instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c B.ByteString where
    decoding = decB64B

decB64B :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c B.ByteString
decB64B = mkDecoding $ implDecodeF (asUnexpected @"enc-B64" . B64.decode)

```
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" .  B64.decode) 


-- specify validFromDec or validFromEnc combinator and use corresponding encoding or decoding function
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c B.ByteString where
    validation = validFromDec decB64B
```


```
recreateAll -> recrAll 
recreateFAll -> recrFAll (forall order changed, first to type variables flipped to nms f c str from f nms)
```

### ToString FromString

Changes in order of type variables (new order is more consistent)
```
-- old
class ToEncString x str f a where
    toEncStringF :: a -> f (Enc '[nm] () str)

-- new
class ToEncString f nm ann a str where
    toEncF :: a -> f (Enc '[nm] () str)

-- old
class FromEncString a f str x where

--

```

```
order of type variables change in backward compatible combinators: 
toEncString
toEncStringF 
fromEncString
fromEncStringF

e.g
-- old
fromEncString :: forall a str nm . (FromEncString a Identity str nm) => Enc '[nm] () str -> a

-- new
fromEncString :: forall nm a str  . (FromEncString Identity nm nm a str) => Enc '[nm] () str -> a

```

Other notes:

- Minor changes in forall variable order in combinators for "r-bool:" encodings.

- Encoder type removed, replaced by Endodings.

- checkWithValidationsEnc -> check

-  `Data.TypedEncoding.Instances.ToEncString.Common` and `Data.TypedEncoding.Instances.Restriction.Common`
   merged into Data.TypedEncoding.Instances.Restriction.Misc