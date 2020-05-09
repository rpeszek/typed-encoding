# Changelog for typed-encoding

## Unreleased changes

## Anticipated future breaking changes

- `Data.TypedEncoding.Internal.Class.IsStringR` expected to be be changed / replaced

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
 
