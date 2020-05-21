

# Conversion Guide from v0.2.x to v0.3.x

## Encoding implementation:

```Haskell
-- v0.2:
instance Applicative f => EncodeF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    encodeF = implEncodeP B64.encode 

-- v0.3:

instance Applicative f => Encode f "enc-B64" "enc-B64" c B.ByteString where
    encoding = encB64B

-- implEncodingP or implEncodingP_ replaces implEncodeP
-- implEncodingEx or implEncodingEx_ replace implEncodeF

encB64B :: Applicative f => Encoding f "enc-B64" "enc-B64" c B.ByteString
encB64B = implEncodingP B64.encode

```

Call site use has not changed, however constraints and order of type parameters (for `-XTypeApplications`) on some of these functions have:


- encodeF - order unchanged     
- encodeAll - order unchanged          
- encodeFAll - forall order changed (first to type variables flipped to `nms f c str` from `f nms`)    

Example:

```
-- old
encodeFAll
  :: EncodeFAll f xs c str =>
     Enc [k] ('[] k) c str
     -> f (Enc [Symbol] xs c str)

-- new
encodeFAll
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
-- old
instance (UnexpectedDecodeErr f, Applicative f) => DecodeF f (Enc ("enc-B64" ': xs) c B.ByteString) (Enc xs c B.ByteString) where
    decodeF = implDecodeF (asUnexpected @"enc-B64" . B64.decode) 


-- new
instance (UnexpectedDecodeErr f, Applicative f) => Decode f "enc-B64" "enc-B64" c B.ByteString where
    decoding = decB64B

-- implDecodingF implDecodingF_ replace implDecodeF
decB64B :: (UnexpectedDecodeErr f, Applicative f) => Decoding f "enc-B64" "enc-B64" c B.ByteString
decB64B = implDecodingF_ (asUnexpected @"enc-B64" . B64.decode)

```

Call site use has not changed, however constraints and order of type parameters (for `-XTypeApplications`) on some of these functions have:

- decodeF - order unchanged 
- decodeAll - order unchanged
- decodeFAll -  forall order changed (first to type variables flipped to `nms f c str` from `f nms`) 


### Validation

```
-- old
instance (RecreateErr f, Applicative f) => RecreateF f (Enc xs c B.ByteString) (Enc ("enc-B64" ': xs) c B.ByteString) where
    checkPrevF = implCheckPrevF (asRecreateErr @"enc-B64" .  B64.decode) 

-- new
-- specify validFromDec or validFromEnc combinator and use corresponding encoding or decoding function
instance (RecreateErr f, Applicative f) => Validate f "enc-B64" "enc-B64" c B.ByteString where
    validation = validFromDec decB64B
```

Call site similar to before forall order changed

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

-- new
class FromEncString f nm ann a str where
```

Call site changes: order of type variables change in backward compatible combinators: 

- toEncString
- toEncStringF 
- fromEncString
- fromEncStringF

e.g.
```
-- old
fromEncString :: forall a str nm . (FromEncString a Identity str nm) => Enc '[nm] () str -> a

-- new
fromEncString :: forall nm a str  . (FromEncString Identity nm nm a str) => Enc '[nm] () str -> a
```

Other notes:

- `Superset` typeclass removed, repaced by `IsSuperset` type family.

- Minor changes in forall variable order in combinators for "r-bool:" encodings.

- Encoder type removed, replaced by Endodings.

- checkWithValidationsEnc -> check

-  `Data.TypedEncoding.Instances.ToEncString.Common` and `Data.TypedEncoding.Instances.Restriction.Common`
   merged into Data.TypedEncoding.Instances.Restriction.Misc

- private `MkCheckedEnc` constructor became `UnsafeMkCheckedEnc`

- big re-org of module folder structure, however two main imports remain largely unchanged:

```
import Data.TypedEncoding
import Data.TypedEncoding.Instances.Support
```