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
    - IsStringR - added reversed IsString class
    - ToEncString - class allowing to convert types to `Enc` strings
    - SomeAnnotation class
    - SomeEnc
 - minor  
    - ToEncString example


## 0.1.0.0
 - initial release
 
