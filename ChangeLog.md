# Changelog for typed-encoding

## Unreleased changes
  - breaking
    - Data.TypedEncoding.Instances modules reorganized
    - Data.TypedEncoding.Internal.Class modules reorganized
    - Data.TypedEncoding.Internal.Utils module renamed
    - Several TypeAnnotations friendly changes:
       * Removed polymorphic kinds in most places
       * Changed typeclass name from 'Subset' to 'Superset'
       * flipped type parameters on FlattenAs, HasA typeclass functions
       * Removed Proxy parameters from several methods (few methods have a '_' backward compatible version which still has them)
  - new functionality
    - 'ToEncString' - class allowing to convert types to `Enc` encoded strings
    - 'FromEncString' - class reverses ToEncString
    - 'SymbolList' utility class
    - 'CheckedEnc' untyped version of `Enc` containing valid encoding
    - 'SomeEnc' existentially quantified version of `Enc` 
    - 'UncheckedEnc' contains not validated encoding
    - 'RecreateExUnkStep' constructor in RecreateEx
    -  utility 'IsStringR' - utility class, reversing 'IsString' class
  - minor  
    - ToEncString example


## 0.1.0.0
 - initial release
 
