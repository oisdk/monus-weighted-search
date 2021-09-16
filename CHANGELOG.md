# Changelog for [`monus-weighted-search` package](https://github.com/oisdk/monus-weighted-search)

## 0.2.0.0

### Instances

   * Removed orphan instance `instance Arbitrary Natural` from exposed code, put
     it in tests directly.
     
     Previously there was an instance for `Arbitrary Natural` in an internal
     module.
     This leaked to users, even though the module itself was internal, so now
     it's been moved out of the package and into the tests entirely.
   
   * Added `instance Monus Any`
   
   * Added `instance (Bounded a, Ord a) => Monus (Max a)`
   
   * Added `Read` instances for `HeapT` and `ListT`

### Performance improvements

   * Removed unnecessary `fmap`s from the implementation of `traverse` on
     `HeapT` and `ListT`
   
   * Coerced some other operations on `HeapT`
   
   * Hand-wrote some `Foldable` methods on `HeapT` and `ListT`
   
### Examples

   * Cleaned up sampling example

## 0.1.0.0

Initial release.
