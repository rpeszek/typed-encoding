# Changelog for typed-encoding

## Anticipated future breaking changes

- `EncodeFAll`, `DecodeFAll`, `RecreateFAll`, `EncodeF`, etc do not work well with more open 
   encoding  annotation such as `"r-ban:soething"` they will be either changed or deprecated / replaced with constructions similar to `Encoder` in `Data.TypedEncoding.Internal.Class.Encoder`.
- `Data.TypedEncoding.Internal.Class.IsStringR` expected to be be changed / replaced
- functions used to create encoding instances or encoding combinators (e.g. `implEncodeP`) will get more constraints. 
- (never ending) rework of internal module stucture to make it easier to navigate 
- Instance and Combinator modules will be merged.
- (post 0.3) "enc-B64" will be moved to a different package (more distant goal)

## Unreleased changes

- breaking
  - deprecated ByteString / Text conversion functions in `Data.TypedEncoding.Instances.Restriction.ASCII`, 
    `Data.TypedEncoding.Instances.Restriction.ASCII` and `Data.TypedEncoding.Instances.Enc.Base64`
    have been removed.
  - Displ String instance (used in examples, has been made consistent with Text and ByteString)

## 0.2.2 

- Next version (0.3) will have number of breaking changes, some rethinking and a lot of cleanup,
  this version preps for some of that (see section above)
- Fixes
  - Conversions type safety issues
  - new and corrected approach to conversions (all old conversion functions have been deprecated)
  - corrected documentation in `Data.TypedEncoding.Combinators.Restriction.BoundedAlphaNums`
- new functionality:
  - `Enc` versions for `pack`/ `unpack` for `Text` and `ByteString`. 
  - `Enc` versions of `decodeUtf8` / `encodeUtf8`
  - new and corrected approach to conversions (all old conversion functions have been deprecated)
  - `IsSuperset` type family with basic combinators deprecates `Superset` typeclass.
  - more modules exported from `Data.TypedEncoding.Instances.Support` for instance and combinator creation
  - more utility type families `Data.TypedEncoding.Internal.Util.TypeLits` 
  - more utility combinators for creating encoding instances and combinators.
  - String instance added in number of places, including for "r-ASCII" encoding
  - few more support convenience functions.
- deprecation warnings (see above) 

## 0.2.1.0

- new functionality:
  - bounded alpha-numeric restriction encodings (`r-ban`)
  - boolean algebra of encodings 
- minor improvements
  - dropped IsString constraint from instances in `Data.TypedEncoding.Instances.Restriction.Common`
  - added forall annotation to encodeAll and decodeAll

## 0.2.0.0

- breaking:
  - Data.TypedEncoding.Instances modules reorganized
  - Data.TypedEncoding.Internal.Class modules reorganized
  - Data.TypedEncoding.Internal.Utils module renamed
  - Several TypeAnnotations friendly changes:
      * Removed polymorphic kinds in most places
      * Changed typeclass name from `Subset` to `Superset`
      * flipped type parameters on FlattenAs, HasA typeclass functions
      * Removed Proxy parameters from several methods (few methods have a '_' backward compatible version which still has them)
- new functionality:
  - `ToEncString` - class allowing to convert types to `Enc` encoded strings
  - `FromEncString` - class reverses ToEncString
  - `CheckedEnc` untyped version of `Enc` containing valid encoding
  - `SomeEnc` existentially quantified version of `Enc` 
  - `UncheckedEnc` for working with not validated encoding
  - `RecreateExUnkStep` constructor added to RecreateEx
  -  utility `IsStringR` - reverse to `IsString` class
  -  utility `SymbolList` class
- docs: 
  - ToEncString example


## 0.1.0.0

- initial release
 
