# Changelog for typed-encoding

## Unreleased changes
 - breaking
    - Data.TypedEncoding.Instances modules reorganized
    - Data.TypedEncoding.Internal.Class modules reorganized
    - Data.TypedEncoding.Internal.Utils module renamed
    - Several TypeAnnotations friendly changes:
       * Removed polymorphic kinds in most places
       * Changed name of Subset to Superset
       * flipped type parameters on FlattenAs, HasA typeclasses
       * Removed Proxy parameters from several methods (few methods have '_' version which still has them)
 - new functionality
    - ToEncString - class allowing to convert types to `Enc` encoded strings
    - FromEncString - reverses ToEncString
    - KnownAnnotation class
    - SomeEnc for untyped (valid) encodings
    - Unchecked type for untyped not validated encodings
    - RecreateExUnkStep constructor in RecreateEx
    - IsStringR - utility class, reversed IsString class
  - minor  
    - ToEncString example


## 0.1.0.0
 - initial release
 
