module List.Extra (minimumBy, maximumBy) where
{-| Convenience functions for working with List

# Common Helpers
@docs maximumBy, minimumBy

# Zipping
@docs zip, zip3

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

{-| Take two lists and returns a list of corresponding pairs
-}
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    (x::xs', y::ys') -> (x,y) :: zip xs' ys'
    (_     , _     ) -> []

{-| Take three lists and returns a list of triples
-}
zip3 : List a -> List b -> List c -> List (a,b,c)
zip3 xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> (x,y,z) :: zip3 xs' ys' zs'
    (_     , _     , _     ) -> []

