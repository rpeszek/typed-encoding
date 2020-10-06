# Changelog for typed-encoding

## Anticipated future breaking changes

- `Data.TypedEncoding.Instances.Do.Sample` will be moved to `Examples`
- `HasA` Typeclass will be moved to `Examples`
- `Data.TypedEncoding.Common.Class.IsStringR` expected to be be changed / replaced
- More module renaming to separate internal implementation code and code targeting examples
- (post 0.5) "enc-B64" will be moved to a different package (more distant goal)
- (intended as private) @implVerifyR@ will be removed from Data.TypedEncoding.Instances.Restriction.UTF8

## 0.5.1.0
- "r-B64" added
- @implVerifyR@ convenience function added to 'Data.TypedEncoding.Instances.Support.Encode'

## 0.5.0

### Changes on a high level

- Most of the changes should not create a big impact on upgrading.  Many definitions were moved to a different module but these modules are and had been re-exported by either `Data.TypedEncoding` or
`Data.TypedEncoding.Instances.Support`
- Some functionality has been moved to Examples or removed, notably:
  - "do-" encodings
  - `SomeEnc`, `SomeAnnotation`
  - `HasA` typeclass
- Some functions have been renamed or type signatures adjusted to follow consistent naming conventions.  In most cases the changes have been made on previously deprecated definitions.


###  Details 

- Data.TypedEncoding.Instances.Do.Sample moved to Examples
- Examples.TypedEncoding folder re-org
- `Data.TypedEncoding.Instances.Support.Helpers` removed `foldEncStr`, `foldCheckedEncStr`
   renamed  `splitSomePayload` to `splitCheckedPayload`
- `HasA` typeclass moved to Examples
- removed experimental `Data.TypedEncoding.Instances.Restriction.Bool` in favor of combinator helpers
  `Data.TypedEncoding.Instances.Support.Bool`
- `Data.TypedEncoding.Common.Types.SomeEnc` moved to Examples  
- `Data.TypedEncoding.Common.Types.SomeAnnotation` moved to Examples
- camel-case of some property names
- Text instances for "Base64" moved to `Data.TypedEncoding.Instances.Enc.Warn.Base64`
- Removed instanced for `"r-()"` encoding
- Functions from `Data.TypedEncoding.Instances.Support.Common` moved to `Data.TypedEncoding.Instances.Support.Decode`
- Signature changed in previously deprecated function `runDecoding` to match `mn ~ alg` convention and deprecation removed
- Signature changed in previously deprecated function `runDecodings` to match `mns ~ algs` convention and deprecation removed
-  Signature changed in previously deprecated function `runValidation` to match `mns ~ algs` convention and deprecation removed
- `runValidationChecks` renamed to `runValidationChecks'` to match /typed-encoding/ naming conventions. 
- removed deprecated `propEncodesInto'`
- moved `Append` type family from from `Data.TypedEncoding.Common.Class.Util` to `Data.TypedEncoding.Common.Util.TypeLits`
- `Data.TypedEncoding.Common.Class.Util` renamed to `Data.TypedEncoding.Common.Class.Common`
- function `extractEither` removed from `Data.TypedEncoding.Internal.Util`
- function `withSomeSymbol` moved to `Data.TypedEncoding.Common.Util.TypeLits`
- function `proxyCons` moved to `Data.TypedEncoding.Common.Util.TypeLits`

- More general instances for some encodings in `Data.TypedEncoding.Instances.Restriction.Misc`
- `mkDecoding` deprecated in favor of `_mkDecoding` to follow the naming convention
- `mkValidation` deprecated in favor of `_mkValidation` to follow the naming convention
- `validR'` function renamed to `_validR`

## 0.4.2

- `Data.TypedEncoding.Instances.Support.Bool` combinators ("r-bool" remains experimental)
- Correction in `r-ban` error message typo ("'G' not bounded by 'F'")
- `_implEncFromString` creates "r-" encoding from `FromEncString` definition
- property changes: `propEncodesIntoCheck` added; `propSuperset'`,  `propEncodesInto'` deprecated

## 0.4.1

- Code Changes. Backward compatible
   - changed order in `IsSuperset` definition to speed up compilation of of more common cases (there is a small chance that it impacts GHC error messages)
   - Faster "r-UTF8", possible issue is changed error message in case ByteString is invalid.
   - Deprecated 'validFromEnc' for its confusing name
   - Deprecated `runDecodings` in favor of consistently named `runDecodings'`
   - Deprecated `runDecoding` in favor of consistently named `runDecoding'`
   - Deprecated `runValidation` in favor of consistently named `runValidation'`

- Documentation / code comment fixes and improvements.

- New functionality `validRFromEnc'` replacing 'validFromEnc' confusing name
  - `propCompEncoding` property
  - `propSupersetCheck` property
  - `propSafeDecoding'` properties
  - `propSafeValidatedDecoding` properties
  - `IsEnc` type family and `Encoding` constraint
  - `getUncheckedPayload` function
  - `pack` and `unpack` overloads in `Data.TypedEncoding.Conv.ByteString.Char8`

- Fixes
  - corrected `propEncodesInto'` property test specification


## 0.4

- Breaking 
  - IsSupersetOpen type family type arguments have changed

- Potentially Breaking  
  (These changes should be backward compatible in almost all cases):
  - Stronger (more precise) constraints on all functions `Data.TypedEncoding.Conv`
  - Compilation errors emitted from `IsSuperset` are different
  - "r-ban" now only allows ASCII chars in annotation name, errors-out otherwise
   
- New
  - `"r-CHAR8"` phantom restriction and `Superset` modified for "r-CHAR8"
  - `"r-UNICODE.D76"` /text/ character set restriction and `Superset` modifications
  - `Superset` constraint added back (different than in 0.2)
  - properties for `Superset` testing
  - `"r-ByteRep"` annotation used as a marker of low level use of `Char` instead of `Word8` for `ByteString` work.

- Improved:
  - `Data.TypedEncoding.Conv` `Text`, `String` and `ByteString` conversions are now more type safe and less error prone.
    Conversion functions are reversible, A to B to C diagrams commute.

- Fixes:
  - `Data.TypedEncoding.Instances.Restriction.BoundedAlphaNums` `"r-ban"` now verifies `Superset "r-ASCII"`
  
## 0.3.0.2

- Added documentation to `Data.TypedEncoding.Conv` outlining current limitations, challenges of conversions.
- Improved readme


## 0.3.0.1

- Documentation changes / corrections
- New _doctest_ tests
- Haddock coverage and @since flags

## 0.3

- Breaking: Numerous changes on the implementation side, new version should be largely compatible on the call site except
    for small differences in constraints and order for type variables (if `-XTypeApplications` is used).
    See [v3 migration guide](https://github.com/rpeszek/typed-encoding/blob/master/doc/v3ConversionGuide.md).
  - `EncodeFAll`, `DecodeFAll`, `RecreateFAll`, `EncodeF`, `DecodeF`, `RecreateF` replaced with
     `EncodeAll`, `DecodeAll`, `ValidateAll`, `Encode`, `Decode`, `Validate`.
  - functions used to create encoding instances or encoding combinators (e.g. `implEncodeP`) are now more precisely typed
  - `Displ String` instance (used in examples, has been made consistent with Text and ByteString)
  - Modules under `Data.TypedEncoding.Combinators` merged into `Data.TypedEncoding.Instances`.
  - Modules under `Data.TypedEncoding.Internal` have been reorganized and moved outside of `Internal`. Various  changes that make the library easier to navigate. 
    for better navigation and discovery.
  - some previously exported combinators (e.g. `implTranF`) have moved to `Data.TypedEncoding.Instances.Support.Unsafe` 
  - `ToEncString`, `FromEncString` have more type variables and function name but backward compatible functions
    have been provided.  
  - `Superset` typeclass removed, replaced with `IsSuperset` type family.
  - Minor changes in `forall` variable order in combinators for `"r-bool:"` encodings.
  - `Encoder` type removed, replaced by `Encodings`.
  - `checkWithValidationsEnc` combinator renamed to `check`
  - (Considered private) `MkCheckedEnc` constructor became `UnsafeMkCheckedEnc`
  - (Considered private) `MkEnc` constructor became `UnsafeMkEnc`
 
- new functionality
  - new types and typeclasses are based on both encoding name and algorithm name allowing
    typeclass definitions for open encodings like `"r-ban:"` that can contain arbitrary symbol literals. 
  - new set of combinators grouped into `_` (compiler decided algorithm), `'` (program specifies algorithm), and
    `algorithm name ~ encoding name` categories  
  - `above` combinator subsumes partial encoding / decoding combinators 
  - `EncodingSuperset` class added


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
 
