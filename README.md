# Convenience functions for working with List

[![Build Status](https://travis-ci.org/elm-community/list-extra.svg?branch=master)](https://travis-ci.org/elm-community/list-extra)

Experimental package with convenience functions for working with List.
Note that this API is experimental and likely to go through many more iterations.

Feedback and contributions are very welcome.

## Run tests

This package uses [elm-test](https://github.com/elm-community/elm-test), please read its documentation to know how to run tests.

## Breaking Changes

**Please Note** that between versions 7.1.0 and 8.0.0 of list `List.Extra` the following breaking changes occured:

1. `replaceIf` was renamed to `setIf`
2. `unzip4` and `unzip5` were removed
3. The type signature of `foldl1` did not change, but the arguments did. `foldl1` takes a `a -> a -> a` as a parameter. However, before version 8.0.0 it had the shape `b -> a -> b` and now it is `a -> b -> b`.
4. The `(!!)` operator was removed, as Elm 0.19 no longer permits custom infix operators 

## Contributing

Pull requests are welcome. You can expect some kind of response within 5 days.

If you are proposing a new function be added, please adhere to the following..

1. Include [documentation](http://package.elm-lang.org/help/documentation-format) and make sure your documentation has a code snippet demonstrating what the function does. We use [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples) in our travis set up which verifies our examples that our example code is correct, so please take advantage of that.
2. Provide a detailed use case where your new function would be useful. Also, compare your new function to the best possible implementation that doesnt include use your function.
3. Add tests to `Tests/Tests.elm`

If you are improving existing functions please demonstrate the performance gains in something like [Ellie](https://ellie-app.com/) and by using a benchmark library like [this one](http://package.elm-lang.org/packages/BrianHicks/elm-benchmark/latest).
