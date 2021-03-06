# Changelog for [`monus-weighted-search` package](https://github.com/oisdk/monus-weighted-search)

## 0.2.0.0

### Data.Monus.Max

   * Added the "max" monus, which allows the types for the various `monusSearch`
     functions to require only `Ord`, and not `Bounded`.

### Control.Monad.Heap

   * The `flatten` function on `HeapT` no longer orders its output.

### Data.Monus

   * Changed second law on `Monus` to be more permissive (now 
     `x <= y ==> x <> (y |-| x) == y`).

### Instances

   * Removed orphan instance `instance Arbitrary Natural` from exposed code, put
     it in tests directly.
     
     Previously there was an instance for `Arbitrary Natural` in an internal
     module.
     This leaked to users, even though the module itself was internal, so now
     it's been moved out of the package and into the tests entirely.
   
   * Added `instance Monus Any`.
   
   * Added `instance (Bounded a, Ord a) => Monus (Max a)`.
   
   * Added `Read` instances for `HeapT` and `ListT`.

   * Added `IsList` instance to `HeapT`.

### Performance improvements

   * Removed unnecessary `fmap`s from the implementation of `traverse` on
     `HeapT` and `ListT`.
   
   * Coerced some other operations on `HeapT`.
   
   * Hand-wrote some `Foldable` methods on `HeapT` and `ListT`.
   
### Examples

   * Cleaned up sampling example.

## 0.1.0.0

Initial release.
