module List.Extra (minimumBy, maximumBy) where
{-| Convenience functions for working with List

# Common Helpers
@docs maximumBy, minimumBy

# Zipping
@docs zip, zip3, zipWith, zipWith3, unzip3

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

{-| Generalise zip by zipping with the function given as the first argument
-}
zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f xs ys =
  case (xs, ys) of
    (x::xs', y::ys') -> f x y :: zipWith f xs' ys'
    (_     , _     ) -> []

{-| Take a function which combines three elements, as well as three lists and returns a list of their point-wise combination
-}
zipWith3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
zipWith3 f xs ys zs =
  case (xs, ys, zs) of
    (x::xs', y::ys', z::zs') -> f x y z :: zipWith3 f xs' ys' zs'
    (_     , _     , _     ) -> []

{-| Take a list of triples and returns three lists
-}
unzip3 : List (a,b,c) -> (List a, List b, List c)
unzip3 triples =
  let step (x,y,z) (xs,ys,zs) =
    (x::xs, y::ys, z::zs)
  in
    foldr step ([], [], []) triples
