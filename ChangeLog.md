# Changelog for typed-encoding

## Anticipated future breaking changes

- ByteString / Text conversion functions in `Data.TypedEncoding.Instances.Restriction.ASCII`, 
  `Data.TypedEncoding.Instances.Restriction.ASCII` and `Data.TypedEncoding.Instances.Enc.Base64`
  are now deprecated and will be removed. 
- `Data.TypedEncoding.Internal.Class.IsStringR` expected to be be changed / replaced
- functions used to create encoding instances or encoding combinators (e.g. `implEncodeP`) will get more constraints. 
- (never ending) rework of internal module stucture to make it easier to navigate 
- module `Data.TypedEncoding.Instances.Support` needs to be imported when working on encoding combinators but name suggests instance work, 
  this needs rethinking. 
- "enc-B64" will be moved to a different package (more distant goal)

## Unreleased changes

- new functionality:
  - `Enc` versions for `pack`/ `unpack` for `Text` and `ByteString`. 
  - `Enc` versions of `decodeUtf8` / `encodeUtf8`
  - new and corrected approach to conversions (all old conversion functions have been deprecated)
  - `IsSuperset` type family with basic combiators deprecates `Superset` typeclass.
  - more modules exported from `Data.TypedEncoding.Instances.Support` for instance and combinator creation
  - more utility type famililes `Data.TypedEncoding.Internal.Util.TypeLits` 
  - more utility combinators for creating encoding instances and combinators.
  - String instance added in number of places, including for "r-ASCII" encoding
  - few more support conveniece functions.
- deprecation warnings (see above) 
- other

## 0.2.1.0

- new functionality:
  - bounded alpha-numeric restriction encodings (`r-ban`)
  - boolean algebra of encodings 
- minor improvements
  - dropped IsString contraint from instances in `Data.TypedEncoding.Instances.Restriction.Common`
  - added forall annotation to ecodeAll and decodeAll

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
 
