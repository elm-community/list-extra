module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple3)
import List exposing (map, range)
import Tuple exposing (first)
import List.Extra exposing (..)


all : Test
all =
    describe "List.Extra"
        [ describe "unique" <|
            [ test "removes duplicates" <|
                \() ->
                    Expect.equal (List.Extra.unique [ 0, 1, 1, 0, 1 ]) [ 0, 1 ]
            , test "preserves list order" <|
                \() ->
                    Expect.equal (List.Extra.unique [ 3, 3, 2, 1, 1, 0 ]) [ 3, 2, 1, 0 ]
            ]
        , describe "allDifferent" <|
            [ test "detects duplicates" <|
                \() ->
                    Expect.equal (List.Extra.allDifferent [ 0, 1, 1, 0, 1 ]) False
            ]
        , describe "andMap" <|
            [ test "computes piecemeal" <|
                \() ->
                    Expect.equal
                        ([ 1, 2, 3 ]
                            |> map (\a b c -> a + b * c)
                            |> andMap [ 4, 5, 6 ]
                            |> andMap [ 2, 1, 1 ]
                        )
                        [ 9, 7, 9 ]
            ]
        , describe "reverseMap" <|
            [ test "maps and reverses" <|
                \() ->
                    Expect.equal (reverseMap sqrt [ 1, 4, 9 ]) [ 3, 2, 1 ]
            ]
        , describe "notMember" <|
            [ test "disconfirms member" <|
                \() ->
                    Expect.equal (notMember 1 [ 1, 2, 3 ]) False
            , test "confirms non-member" <|
                \() ->
                    Expect.equal (notMember 4 [ 1, 2, 3 ]) True
            ]
        , describe "find" <|
            [ test "behaves as documented" <|
                \() ->
                    Expect.equal (find (\num -> num > 5) [ 2, 4, 6, 8 ]) (Just 6)
            ]
        , describe "elemIndex" <|
            [ test "finds index of value" <|
                \() ->
                    Expect.equal (elemIndex 1 [ 1, 2, 3 ]) (Just 0)
            , test "doesn't find index of non-present" <|
                \() ->
                    Expect.equal (elemIndex 4 [ 1, 2, 3 ]) Nothing
            , test "finds index of first match" <|
                \() ->
                    Expect.equal (elemIndex 1 [ 1, 2, 1 ]) (Just 0)
            ]
        , describe "elemIndices" <|
            [ test "finds singleton index" <|
                \() ->
                    Expect.equal (elemIndices 1 [ 1, 2, 3 ]) [ 0 ]
            , test "doesn't find indices of non-present" <|
                \() ->
                    Expect.equal (elemIndices 4 [ 1, 2, 3 ]) []
            , test "finds all indices" <|
                \() ->
                    Expect.equal (elemIndices 1 [ 1, 2, 1 ]) [ 0, 2 ]
            ]
        , describe "findIndex" <|
            [ test "finds index of value" <|
                \() ->
                    Expect.equal (findIndex (\x -> x % 2 == 0) [ 1, 2, 3 ]) (Just 1)
            , test "doesn't find index of non-present" <|
                \() ->
                    Expect.equal (findIndex (\x -> x % 2 == 0) [ 1, 3, 5 ]) Nothing
            , test "finds index of first match" <|
                \() ->
                    Expect.equal (findIndex (\x -> x % 2 == 0) [ 1, 2, 4 ]) (Just 1)
            ]
        , describe "findIndices" <|
            [ test "finds singleton index" <|
                \() ->
                    Expect.equal (findIndices (\x -> x % 2 == 0) [ 1, 2, 3 ]) [ 1 ]
            , test "doesn't find indices of non-present" <|
                \() ->
                    Expect.equal (findIndices (\x -> x % 2 == 0) [ 1, 3, 5 ]) []
            , test "finds all indices" <|
                \() ->
                    Expect.equal (findIndices (\x -> x % 2 == 0) [ 1, 2, 4 ]) [ 1, 2 ]
            ]
        , describe "count" <|
            [ test "isOdd predicate" <|
                \() ->
                    Expect.equal (count (\n -> n % 2 == 1) [ 1, 2, 3, 4, 5, 6, 7 ]) 4
            , test "equal predicate" <|
                \() ->
                    Expect.equal (count ((==) "yeah") [ "She", "loves", "you", "yeah", "yeah", "yeah" ]) 3
            ]
        , describe "intercalate" <|
            [ test "computes example" <|
                \() ->
                    Expect.equal
                        (intercalate [ 0, 0 ] [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ])
                        [ 1, 2, 0, 0, 3, 4, 0, 0, 5, 6 ]
            ]
        , describe "transpose" <|
            [ test "performs basic transpose" <|
                \() ->
                    Expect.equal
                        (transpose [ [ 1, 2, 3 ], [ 4, 5, 6 ] ])
                        [ [ 1, 4 ], [ 2, 5 ], [ 3, 6 ] ]
            , test "truncate the matrix to the shortest row size" <|
                \() ->
                    Expect.equal
                        (transpose [ [ 10, 11 ], [ 20 ], [ 30, 31, 32 ] ])
                        [ [ 10, 20, 30 ] ]
            , test "transposes large lists" <|
                \() ->
                    Expect.equal
                        (transpose [ List.repeat 10000 1 ])
                        (List.repeat 10000 [ 1 ])
            ]
        , describe "subsequences" <|
            [ test "computes subsequences" <|
                \() ->
                    Expect.equal
                        (subsequences [ 1, 2, 3 ])
                        [ [], [ 1 ], [ 2 ], [ 1, 2 ], [ 3 ], [ 1, 3 ], [ 2, 3 ], [ 1, 2, 3 ] ]
            ]
        , describe "permutations" <|
            [ test "computes permutations" <|
                \() ->
                    Expect.equal
                        (permutations [ 1, 2, 3 ])
                        [ [ 1, 2, 3 ], [ 1, 3, 2 ], [ 2, 1, 3 ], [ 2, 3, 1 ], [ 3, 1, 2 ], [ 3, 2, 1 ] ]
            ]
        , describe "interweave" <|
            [ test "interweaves lists of equal length" <|
                \() ->
                    Expect.equal (interweave [ 1, 3 ] [ 2, 4 ]) [ 1, 2, 3, 4 ]
            , test "appends remaining members of first list, if longer" <|
                \() ->
                    Expect.equal (interweave [ 1, 3, 5, 7 ] [ 2, 4 ]) [ 1, 2, 3, 4, 5, 7 ]
            , test "appends remaining members of second list, if longer" <|
                \() ->
                    Expect.equal (interweave [ 4, 9, 16 ] [ 2, 3, 5, 7 ]) [ 4, 2, 9, 3, 16, 5, 7 ]
            ]
        , describe "cartesianProduct" <|
            [ test "computes the cartesian product of lists of different length" <|
                \() ->
                    Expect.equal (cartesianProduct [ [ 1, 2 ], [ 3, 4, 5 ], [ 6 ] ]) [ [ 1, 3, 6 ], [ 1, 4, 6 ], [ 1, 5, 6 ], [ 2, 3, 6 ], [ 2, 4, 6 ], [ 2, 5, 6 ] ]
            , test "computes the cartesian product of a single list" <|
                \() ->
                    Expect.equal (cartesianProduct [ [ 1, 2 ] ]) [ [ 1 ], [ 2 ] ]
            , test "computes the cartesian product of lists including an empty one" <|
                \() ->
                    Expect.equal (cartesianProduct [ [ 1, 2 ], [], [ 6 ] ]) []
            , test "computes the nullary cartesian product" <|
                \() ->
                    Expect.equal (cartesianProduct []) [ [] ]
            ]
        , describe "foldl1" <|
            [ test "computes maximum" <|
                \() ->
                    Expect.equal (foldl1 max [ 1, 2, 3, 2, 1 ]) (Just 3)
            , test "falls back to Nothing" <|
                \() ->
                    Expect.equal (foldl1 max []) Nothing
            , test "computes left to right difference" <|
                \() ->
                    Expect.equal (foldl1 (-) [ 1, 2, 3 ]) (Just -4)
            ]
        , describe "foldr1" <|
            [ test "computes minimum" <|
                \() ->
                    Expect.equal (foldr1 min [ 1, 2, 3, 2, 1 ]) (Just 1)
            , test "falls back to Nothing" <|
                \() ->
                    Expect.equal (foldr1 min []) Nothing
            , test "computes right to left difference" <|
                \() ->
                    Expect.equal (foldr1 (-) [ 1, 2, 3 ]) (Just 2)
            ]
        , describe "scanl1" <|
            [ test "computes left to right iterative sum" <|
                \() ->
                    Expect.equal (scanl1 (+) [ 1, 2, 3 ]) [ 1, 3, 6 ]
            , test "computes left to right iterative difference" <|
                \() ->
                    Expect.equal (scanl1 (-) [ 1, 2, 3 ]) [ 1, 1, 2 ]
            , test "computes left to right flipped iterative difference" <|
                \() ->
                    Expect.equal (scanl1 (flip (-)) [ 1, 2, 3 ]) [ 1, -1, -4 ]
            ]
        , describe "scanr" <|
            [ test "computes right to left iterative sum" <|
                \() ->
                    Expect.equal (scanr (+) 0 [ 1, 2, 3 ]) [ 6, 5, 3, 0 ]
            , test "computes right to left iterative difference" <|
                \() ->
                    Expect.equal (scanr (-) 0 [ 1, 2, 3 ]) [ 2, -1, 3, 0 ]
            ]
        , describe "scanr1" <|
            [ test "computes right to left iterative sum" <|
                \() ->
                    Expect.equal (scanr1 (+) [ 1, 2, 3 ]) [ 6, 5, 3 ]
            , test "computes right to left iterative difference" <|
                \() ->
                    Expect.equal (scanr1 (-) [ 1, 2, 3 ]) [ 2, -1, 3 ]
            , test "computes right to left flipped iterative difference" <|
                \() ->
                    Expect.equal (scanr1 (flip (-)) [ 1, 2, 3 ]) [ 0, 1, 3 ]
            ]
        , describe "mapAccuml" <|
            [ test "on empty list" <|
                \() ->
                    Expect.equal
                        (mapAccuml (\a x -> ( a + x, a * x )) 5 [])
                        ( 5, [] )
            , test "accumulate sum and map product" <|
                \() ->
                    Expect.equal
                        (mapAccuml (\a x -> ( a + x, a * x )) 5 [ 2, 4, 8 ])
                        ( 19, [ 10, 28, 88 ] )
            , test "running total" <|
                \() ->
                    Expect.equal
                        (mapAccuml (\a x -> ( a + x, ( x, a + x ) )) 0 [ 2, 4, 8 ])
                        ( 14, [ ( 2, 2 ), ( 4, 6 ), ( 8, 14 ) ] )
            , test "works for very long list (i.e. is call stack size safe)" <|
                \() ->
                    Expect.equal
                        (mapAccuml (\a x -> ( a + x, () )) 0 (List.range 1 100000) |> Tuple.first)
                        5000050000
            ]
        , describe "mapAccumr" <|
            [ test "on empty list" <|
                \() ->
                    Expect.equal
                        (mapAccumr (\a x -> ( a + x, a * x )) 5 [])
                        ( 5, [] )
            , test "accumulate sum and map product" <|
                \() ->
                    Expect.equal
                        (mapAccumr (\a x -> ( a + x, a * x )) 5 [ 2, 4, 8 ])
                        ( 19, [ 34, 52, 40 ] )
            , test "add count down" <|
                \() ->
                    Expect.equal
                        (mapAccumr (\a x -> ( a + 1, ( x, a ) )) 0 [ 2, 4, 8 ])
                        ( 3, [ ( 2, 2 ), ( 4, 1 ), ( 8, 0 ) ] )
            , test "works for very long list (i.e. is call stack size safe)" <|
                \() ->
                    Expect.equal
                        (mapAccumr (\a x -> ( a + x, () )) 0 (List.range 1 100000) |> Tuple.first)
                        5000050000
            ]
        , describe "unfoldr" <|
            [ test "builds a decreasing list from a seed" <|
                \() ->
                    Expect.equal
                        (unfoldr
                            (\b ->
                                if b == 0 then
                                    Nothing
                                else
                                    Just ( b, b - 1 )
                            )
                            5
                        )
                        [ 5, 4, 3, 2, 1 ]
            ]
        , describe "iterate" <|
            [ test "collatz 13" <|
                \() ->
                    let
                        collatz n =
                            if n == 1 then
                                Nothing
                            else
                                Just <|
                                    if n % 2 == 0 then
                                        n / 2
                                    else
                                        3 * n + 1
                    in
                        Expect.equal (iterate collatz 13) [ 13, 40, 20, 10, 5, 16, 8, 4, 2, 1 ]
            ]
        , describe "initialize" <|
            [ test "creates a list starting from zero" <|
                \() ->
                    Expect.equal (initialize 5 identity) [ 0, 1, 2, 3, 4 ]
            , test "creates a list by doubling the index" <|
                \() ->
                    Expect.equal (initialize 5 (\x -> x * 2)) [ 0, 2, 4, 6, 8 ]
            , test "creates a list of identical values" <|
                \() ->
                    Expect.equal (initialize 1 (always 3)) [ 3 ]
            ]
        , describe "cycle" <|
            [ test "same length" <|
                \() ->
                    Expect.equal (cycle 3 [ 4, 7, 8 ]) [ 4, 7, 8 ]
            , test "multiple of length" <|
                \() ->
                    Expect.equal (cycle 6 [ 4, 7, 8 ]) [ 4, 7, 8, 4, 7, 8 ]
            , test "partial cycle" <|
                \() ->
                    Expect.equal (cycle 4 [ 'a', 'b', 'c' ]) [ 'a', 'b', 'c', 'a' ]
            , test "empty list" <|
                \() ->
                    Expect.equal (cycle 9001 []) []
            , test "resulting length smaller than cycle length" <|
                \() ->
                    Expect.equal (cycle 2 [ 1, 2, 3, 4, 5 ]) [ 1, 2 ]
            ]
        , describe "splitAt" <|
            [ test "splits a list in the middle" <|
                \() ->
                    Expect.equal (splitAt 3 [ 1, 2, 3, 4, 5 ]) ( [ 1, 2, 3 ], [ 4, 5 ] )
            , test "splits a list at the first element" <|
                \() ->
                    Expect.equal (splitAt 1 [ 1, 2, 3 ]) ( [ 1 ], [ 2, 3 ] )
            , test "splits the entire list correctly" <|
                \() ->
                    Expect.equal (splitAt 3 [ 1, 2, 3 ]) ( [ 1, 2, 3 ], [] )
            , test "splits past the length of the list" <|
                \() ->
                    Expect.equal (splitAt 4 [ 1, 2, 3 ]) ( [ 1, 2, 3 ], [] )
            , test "handles zero correctly" <|
                \() ->
                    Expect.equal (splitAt 0 [ 1, 2, 3 ]) ( [], [ 1, 2, 3 ] )
            , test "handles negative numbers correctly" <|
                \() ->
                    Expect.equal (splitAt (-1) [ 1, 2, 3 ]) ( [], [ 1, 2, 3 ] )
            ]
        , describe "splitWhen" <|
            [ test "returns split list when predicate is true" <|
                \() ->
                    Expect.equal (splitWhen (\n -> n == 3) [ 1, 2, 3, 4, 5 ]) (Just ( [ 1, 2 ], [ 3, 4, 5 ] ))
            , test "returns nothing when predicate is false" <|
                \() ->
                    Expect.equal (splitWhen (\n -> n == 6) [ 1, 2, 3, 4, 5 ]) Nothing
            ]
        , describe "takeWhileRight" <|
            [ test "keeps the correct items" <|
                \() ->
                    Expect.equal (takeWhileRight ((<) 5) (range 1 10)) [ 6, 7, 8, 9, 10 ]
            , test "drops the correct items" <|
                \() ->
                    Expect.equal (dropWhileRight ((<) 5) (range 1 10)) [ 1, 2, 3, 4, 5 ]
            ]
        , describe "takeWhile" <|
            [ test "doesn't exceed maximum call stack" <|
                \() ->
                    Expect.equal (takeWhile ((>) 19999) (range 1 20000)) (range 1 19998)
            ]
        , describe "span" <|
            [ test "splits in the middle of the list" <|
                \() ->
                    Expect.equal (span ((>) 3) [ 1, 2, 3, 4, 1, 2, 3, 4 ]) ( [ 1, 2 ], [ 3, 4, 1, 2, 3, 4 ] )
            , test "every element passes predicate" <|
                \() ->
                    Expect.equal (span ((>) 5) [ 1, 2, 3 ]) ( [ 1, 2, 3 ], [] )
            , test "first item doesn't pass predicate" <|
                \() ->
                    Expect.equal (span ((>) 0) [ 1, 2, 3 ]) ( [], [ 1, 2, 3 ] )
            ]
        , describe "break" <|
            [ test "breaks in the middle of the list" <|
                \() ->
                    Expect.equal (break ((<) 3) [ 1, 2, 3, 4, 1, 2, 3, 4 ]) ( [ 1, 2, 3 ], [ 4, 1, 2, 3, 4 ] )
            , test "breaks on the first item" <|
                \() ->
                    Expect.equal (break ((>) 5) [ 1, 2, 3 ]) ( [], [ 1, 2, 3 ] )
            , test "doesn't break for any element" <|
                \() ->
                    Expect.equal (break ((<) 5) [ 1, 2, 3 ]) ( [ 1, 2, 3 ], [] )
            ]
        , describe "stripPrefix" <|
            [ test "removes a matching prefix" <|
                \() ->
                    Expect.equal (stripPrefix [ 1, 2 ] [ 1, 2, 3, 4 ]) (Just [ 3, 4 ])
            , test "removes a matching 3-element prefix" <|
                \() ->
                    Expect.equal (stripPrefix [ 1, 2, 3 ] [ 1, 2, 3, 4, 5 ]) (Just [ 4, 5 ])
            , test "can remove the entire list" <|
                \() ->
                    Expect.equal (stripPrefix [ 1, 2, 3 ] [ 1, 2, 3 ]) (Just [])
            , test "fails when prefix is longer than list" <|
                \() ->
                    Expect.equal (stripPrefix [ 1, 2, 3 ] [ 1, 2 ]) Nothing
            , test "fails when list doesn't contain prefix" <|
                \() ->
                    Expect.equal (stripPrefix [ 3, 2, 1 ] [ 1, 2, 3, 4, 5 ]) Nothing
            ]
        , describe "group" <|
            [ test "groups elements correctly" <|
                \() ->
                    Expect.equal (group [ 1, 2, 2, 3, 3, 3, 2, 2, 1 ]) [ [ 1 ], [ 2, 2 ], [ 3, 3, 3 ], [ 2, 2 ], [ 1 ] ]
            ]
        , describe "groupWhile" <|
            [ test "groups by sub-element equality" <|
                \() ->
                    Expect.equal
                        (groupWhile (\x y -> first x == first y) [ ( 0, 'a' ), ( 0, 'b' ), ( 1, 'c' ), ( 1, 'd' ) ])
                        [ [ ( 0, 'a' ), ( 0, 'b' ) ], [ ( 1, 'c' ), ( 1, 'd' ) ] ]
            , test "comparison function is reflexive, symmetric, and transitive" <|
                \() ->
                    Expect.equal
                        (groupWhile (<) [ 1, 2, 3, 2, 4, 1, 3, 2, 1 ])
                        [ [ 1, 2, 3, 2, 4 ], [ 1, 3, 2 ], [ 1 ] ]
            ]
        , describe "groupWhileTransitively" <|
            [ test "an empty list" <|
                \() ->
                    Expect.equal
                        (groupWhileTransitively (<) [])
                        []
            , test "a single item" <|
                \() ->
                    Expect.equal
                        (groupWhileTransitively (<) [ 1 ])
                        [ [ 1 ] ]
            , test "a standard working case" <|
                \() ->
                    Expect.equal
                        (groupWhileTransitively (<) [ 1, 2, 3, 2, 4, 1, 3, 2, 1 ])
                        [ [ 1, 2, 3 ], [ 2, 4 ], [ 1, 3 ], [ 2 ], [ 1 ] ]
            ]
        , describe "inits" <|
            [ test "returns all initial segments" <|
                \() ->
                    Expect.equal (inits [ 1, 2, 3 ]) [ [], [ 1 ], [ 1, 2 ], [ 1, 2, 3 ] ]
            ]
        , describe "tails" <|
            [ test "returns all final segments" <|
                \() ->
                    Expect.equal (tails [ 1, 2, 3 ]) [ [ 1, 2, 3 ], [ 2, 3 ], [ 3 ], [] ]
            ]
        , describe "select" <|
            [ test "returns all variations with a single item removed" <|
                \() ->
                    Expect.equal
                        (select [ 1, 2, 3, 4 ])
                        [ ( 1, [ 2, 3, 4 ] ), ( 2, [ 1, 3, 4 ] ), ( 3, [ 1, 2, 4 ] ), ( 4, [ 1, 2, 3 ] ) ]
            ]
        , describe "selectSplit" <|
            [ test "returns all splits at a single item" <|
                \() ->
                    Expect.equal (selectSplit [ 1, 2, 3 ]) [ ( [], 1, [ 2, 3 ] ), ( [ 1 ], 2, [ 3 ] ), ( [ 1, 2 ], 3, [] ) ]
            ]
        , describe "isSubsequenceOf" <|
            [ test "success" <|
                \() ->
                    Expect.true "Elm is a subsequence of Eat lime" (isSubsequenceOf [ "E", "l", "m" ] [ "E", "a", "t", " ", "l", "i", "m", "e", "s" ])
            , test "failure" <|
                \() ->
                    Expect.false "Elm is not a subsequence of Email" (isSubsequenceOf [ "E", "l", "m" ] [ "E", "m", "a", "i", "l" ])
            , test "success at last element" <|
                \() ->
                    Expect.true "[] should be a subsequence of []" (isSubsequenceOf [ 1, 3 ] [ 1, 2, 3 ])
            ]
        , describe "lift2" <|
            [ test "produces all combinations of addition" <|
                \() ->
                    Expect.equal (lift2 (+) [ 1, 2, 3 ] [ 4, 5 ]) [ 5, 6, 6, 7, 7, 8 ]
            ]
        , describe "groupsOf" <|
            [ test "groups by the correct number of items" <|
                \() ->
                    Expect.equal (groupsOf 3 (range 1 10))
                        [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ] ]
            ]
        , describe "groupsOfWithStep" <|
            [ test "step == size" <|
                \() ->
                    Expect.equal (groupsOfWithStep 4 4 (range 1 10))
                        [ [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ] ]
            , test "step < size" <|
                \() ->
                    Expect.equal (groupsOfWithStep 3 1 (range 1 5))
                        [ [ 1, 2, 3 ], [ 2, 3, 4 ], [ 3, 4, 5 ] ]
            , test "step > size" <|
                \() ->
                    Expect.equal (groupsOfWithStep 3 6 (range 1 20))
                        [ [ 1, 2, 3 ], [ 7, 8, 9 ], [ 13, 14, 15 ] ]
            ]
        , describe "groupsOfVarying" <|
            [ test "group sizes match passed sizes" <|
                \() ->
                    Expect.equal
                        (groupsOfVarying [ 2, 3, 1 ] [ "a", "b", "c", "d", "e", "f" ])
                        [ [ "a", "b" ], [ "c", "d", "e" ], [ "f" ] ]
            , test "groups correctly when passed counts are less than list size" <|
                \() ->
                    Expect.equal
                        (groupsOfVarying [ 2 ] [ "a", "b", "c", "d", "e", "f" ])
                        [ [ "a", "b" ] ]
            , test "groups correctly when passed counts are greater than list size" <|
                \() ->
                    Expect.equal
                        (groupsOfVarying [ 2, 3, 1, 5, 6 ] [ "a", "b", "c", "d", "e" ])
                        [ [ "a", "b" ], [ "c", "d", "e" ] ]
            ]
        , describe "greedyGroupsOf" <|
            [ test "groups correctly while keeping trailing group" <|
                \() ->
                    Expect.equal (greedyGroupsOf 3 (range 1 10))
                        [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7, 8, 9 ], [ 10 ] ]
            ]
        , describe "greedyGroupsOfWithStep" <|
            [ test "step == size" <|
                \() ->
                    Expect.equal (greedyGroupsOfWithStep 4 4 (range 1 10))
                        [ [ 1, 2, 3, 4 ], [ 5, 6, 7, 8 ], [ 9, 10 ] ]
            , test "step < size" <|
                \() ->
                    Expect.equal (greedyGroupsOfWithStep 3 2 (range 1 6))
                        [ [ 1, 2, 3 ], [ 3, 4, 5 ], [ 5, 6 ] ]
            , test "step > size" <|
                \() ->
                    Expect.equal (greedyGroupsOfWithStep 3 6 (range 1 20))
                        [ [ 1, 2, 3 ], [ 7, 8, 9 ], [ 13, 14, 15 ], [ 19, 20 ] ]
            ]
        , describe "isPrefixOf"
            [ fuzz (list int) "[] is prefix to anything" <|
                \list ->
                    List.Extra.isPrefixOf [] list
                        |> Expect.true "Expected [] to be a prefix."
            , fuzz (list int) "reflexivity" <|
                \list ->
                    List.Extra.isPrefixOf list list
                        |> Expect.true "Expected list to be a prefix of itself."
            , fuzz2 (list int) (list int) "antisymmetry" <|
                \listA listB ->
                    not (List.Extra.isPrefixOf listA listB)
                        || not (List.Extra.isPrefixOf listB listA)
                        || listA
                        == listB
                        |> Expect.true "Expected exactly one to be prefix of the other."
            , fuzz3 (list int) (list int) (list int) "transitivity" <|
                \listA listB listC ->
                    not (List.Extra.isPrefixOf listA listB)
                        || not (List.Extra.isPrefixOf listB listC)
                        || List.Extra.isPrefixOf listA listC
                        |> Expect.true "Expected prefix of prefix to be prefix."
            ]
        , describe "isSuffixOf"
            [ fuzz (list int) "[] is suffix to anything" <|
                \list ->
                    List.Extra.isSuffixOf [] list
                        |> Expect.true "Expected [] to be a suffix."
            , fuzz (list int) "reflexivity" <|
                \list ->
                    List.Extra.isSuffixOf list list
                        |> Expect.true "Expected list to be a suffix of itself."
            , fuzz2 (list int) (list int) "antisymmetry" <|
                \listA listB ->
                    not (List.Extra.isSuffixOf listA listB)
                        || not (List.Extra.isSuffixOf listB listA)
                        || listA
                        == listB
                        |> Expect.true "Expected exactly one to be suffix of the other."
            , fuzz3 (list int) (list int) (list int) "transitivity" <|
                \listA listB listC ->
                    not (List.Extra.isSuffixOf listA listB)
                        || not (List.Extra.isSuffixOf listB listC)
                        || List.Extra.isSuffixOf listA listC
                        |> Expect.true "Expected suffix of suffix to be suffix."
            ]
        , describe "isInfixOf"
            [ test "success" <|
                \() ->
                    Expect.true "5, 7, 11 is infix of 2, 3, 5, 7, 11, 13" (isInfixOf [ 5, 7, 11 ] [ 2, 3, 5, 7, 11, 13 ])
            , test "not consecutive" <|
                \() ->
                    Expect.false "5, 7, 13 is not infix of 2, 3, 5, 7, 11, 13" (isInfixOf [ 5, 7, 13 ] [ 2, 3, 5, 7, 11, 13 ])
            , test "not in-order" <|
                \() ->
                    Expect.false "3, 5, 2 is not infix of 2, 3, 5, 7, 11, 13" (isInfixOf [ 3, 5, 2 ] [ 2, 3, 5, 7, 11, 13 ])
            ]
        , describe "swapAt"
            [ test "negative index as first argument returns the original list" <|
                \() ->
                    Expect.equal (swapAt -1 0 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "negative index as second argument returns the original list" <|
                \() ->
                    Expect.equal (swapAt 0 -1 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "out of range index as first argument returns the original list" <|
                \() ->
                    Expect.equal (swapAt 10 0 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "out of range index as second argument returns the original list" <|
                \() ->
                    Expect.equal (swapAt 0 -1 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "identical indexes returns the original list" <|
                \() ->
                    Expect.equal (swapAt 1 1 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "swap the elements at indices 0 and 1" <|
                \() ->
                    Expect.equal (swapAt 0 1 [ 1, 2, 3 ]) [ 2, 1, 3 ]
            ]
        , describe "removeAt"
            [ test "negative index returns the original list" <|
                \() ->
                    Expect.equal (removeAt -1 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "remove the element at index 0" <|
                \() ->
                    Expect.equal (removeAt 0 [ 1, 2, 3 ]) [ 2, 3 ]
            , test "remove the element at index 2" <|
                \() ->
                    Expect.equal (removeAt 2 [ 1, 2, 3 ]) [ 1, 2 ]
            , test "out of range index returns the original list" <|
                \() ->
                    Expect.equal (removeAt 4 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            ]
        , describe "setAt"
            [ test "negative index returns the original list" <|
                \() ->
                    Expect.equal (setAt -1 9 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "set index 0 to 9" <|
                \() ->
                    Expect.equal (setAt 0 9 [ 1, 2, 3 ]) [ 9, 2, 3 ]
            , test "set index 2 to 9" <|
                \() ->
                    Expect.equal (setAt 2 9 [ 1, 2, 3 ]) [ 1, 2, 9 ]
            , test "out of range index returns the original list" <|
                \() ->
                    Expect.equal (setAt 4 9 [ 1, 2, 3 ]) [ 1, 2, 3 ]
            ]
        , describe "updateAt"
            [ test "negative index returns the original list" <|
                \() ->
                    Expect.equal (updateAt -1 ((+) 1) [ 1, 2, 3 ]) [ 1, 2, 3 ]
            , test "increment the element at index 0" <|
                \() ->
                    Expect.equal (updateAt 0 ((+) 1) [ 1, 2, 3 ]) [ 2, 2, 3 ]
            , test "increment the element at index 2" <|
                \() ->
                    Expect.equal (updateAt 2 ((+) 1) [ 1, 2, 3 ]) [ 1, 2, 4 ]
            , test "out of range index returns the original list" <|
                \() ->
                    Expect.equal (updateAt 4 ((+) 1) [ 1, 2, 3 ]) [ 1, 2, 3 ]
            ]
        , describe "updateIfIndex"
            [ test "increments first element" <|
                \() ->
                    Expect.equal (updateIfIndex (always True) ((+) 1) [ 1, 2, 3 ]) [ 2, 3, 4 ]
            , test "if the index is 2, then increment the element" <|
                \() ->
                    Expect.equal (updateIfIndex ((==) 2) ((+) 1) [ 1, 2, 3 ]) [ 1, 2, 4 ]
            , test "if the index is even, increment the element" <|
                \() ->
                    Expect.equal (updateIfIndex (\index -> index % 2 == 0) ((+) 1) [ 1, 2, 3 ]) [ 2, 2, 4 ]
            ]
        , describe "removeIfIndex"
            [ test "remove all the elements" <|
                \() ->
                    Expect.equal (removeIfIndex (always True) [ 1, 2, 3 ]) []
            , test "remove the element at index 2" <|
                \() ->
                    Expect.equal (removeIfIndex ((==) 2) [ 1, 2, 3 ]) [ 1, 2 ]
            , test "remove all elements at even indices" <|
                \() ->
                    Expect.equal (removeIfIndex (\index -> index % 2 == 0) [ 1, 2, 3 ]) [ 2 ]
            ]
        ]
