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