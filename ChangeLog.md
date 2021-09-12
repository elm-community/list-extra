### Next
1. `unique` and `uniqueBy` functions no longer requires comparable input arguments. This change increased performance for lists sized 0-100 elements by about 40% but decreases performance for lists sized >200 by about 35%. Seems like a worthy trade off.
2. `isPermutationOf` is vastly more performant. @lue-bird 's benchmarks show cases of being 141,000% more performant. 

### 8.4.0
1. New function "joinOn"

### 8.3.2
1. isInfixOf is tail call optimized

### 8.3.1
1. gatherWith is tail call optimized

### 8.3.0
1. New function `findMap`

### 8.2.4
1. Speed up removeAt implementation
2. Make iterate tail recursive

### 8.2.3
1. Fixed bug in `isInfixOf` that prevented the detection of infixes after a partial infix (Issue #132)

### 8.2.0
Notes
1. `groupWhile` implementation rewritten to be tail-recursive to eliminate risk of stack overflow errors. Trade off is its about 12% slower.

Additions
1. `pairings`

### 8.1.0
Notes
1. Documentation cleaned up, and Elm verify examples implemented

Additions
1. `gatherEquals`, `gatherEqualsBy`, and `gatherWith` 
2. `maximumWith` and `minimumWith`

### 8.0.0 
Notes
1. First Elm 0.19 release

Breaking Changes
1. `replaceIf` was renamed to `setIf`
2. `unzip4` and `unzip5` were removed
3. The `(!!)` operator was removed, as Elm 0.19 no longer permits custom infix operators 
4. *Really Breaky*: The type signature of `foldl1` did not change, but the arguments did. `foldl1` takes a `a -> a -> a` as a parameter. However, before version 8.0.0 it had the shape `b -> a -> b` and now it is `a -> b -> b`.
