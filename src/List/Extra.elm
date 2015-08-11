module List.Extra
  ( minimumBy
  , maximumBy
  , andMap
  , takeWhile
  , dropWhile
  , dropDuplicates
  , find
  , replaceIf
  , zip
  , zip3
  , zip4
  , zip5
  ) where
{-| Convenience functions for working with List

# Common Helpers
@docs maximumBy, minimumBy, andMap, takeWhile, dropWhile, dropDuplicates, find, replaceIf

# Zipping
@docs zip, zip3, zip4, zip5

-}

import List exposing (..)
import Set exposing (member)

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

{-| Take elements in order as long as the predicate evaluates to `True`
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if | (predicate x) -> x :: takeWhile predicate xs
                  | otherwise -> []

{-| Drop elements in order as long as the predicate evaluates to `True`
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if | (predicate x) -> dropWhile predicate xs
                  | otherwise -> list

{-| Drop _all_ duplicate elements from the list
-}
dropDuplicates : List comparable -> List comparable
dropDuplicates list =
  let
    step next (set, acc) =
      if Set.member next set
        then (set, acc)
        else (Set.insert next set, next::acc)
  in
    List.foldl step (Set.empty, []) list |> snd |> List.reverse

{-| Map functions taking multiple arguments over multiple lists. Each list should be of the same length.

    ( (\a b c -> a + b * c)
        `map` [1,2,3]
        `andMap` [4,5,6]
        `andMap` [2,1,1]
    ) == [9,7,9]
-}
andMap : List (a -> b) -> List a -> List b
andMap fl l = map2 (<|) fl l


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [2, 4, 6, 8] == Just 6
-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first::rest ->
            if predicate first then
                Just first
            else
                find predicate rest

{-| Replace all values that satisfy a predicate with a replacement value.
-}
replaceIf : (a -> Bool) -> a -> List a -> List a
replaceIf predicate replacement list =
  List.map (\item -> if predicate item then replacement else item) list

{-| Take two lists and returns a list of corresponding pairs
-}
zip : List a -> List b -> List (a,b)
zip = map2 (,)

{-| Take three lists and returns a list of triples
-}
zip3 : List a -> List b -> List c -> List (a,b,c)
zip3 = map3 (,,)

{-| Take four lists and returns a list of quadruples
-}
zip4 : List a -> List b -> List c -> List d -> List (a,b,c,d)
zip4 = map4 (,,,)

{-| Take five lists and returns a list of quintuples
-}
zip5 : List a -> List b -> List c -> List d -> List e -> List (a,b,c,d,e)
zip5 = map5 (,,,,)
