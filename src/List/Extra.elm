module List.Extra (minimumBy, maximumBy) where
{-| Convenience functions for working with List

# Common Helpers
@docs maximumBy, minimumBy

-}

import List exposing (..)

{-| Find the first maximum element in a list using a comparable transformation
-}
maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f ls =
  let maxBy f x y = if (f x) > (f y) then x else y
  in case ls of
        l'::ls' -> Just <| foldl (maxBy f) l' ls'
        _       -> Nothing

{-| Find the first minimum element in a list using a comparable transformation
-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
  let minBy f x y = if (f x) < (f y) then x else y
  in case ls of
        l'::ls' -> Just <| foldl (minBy f) l' ls'
        _       -> Nothing
