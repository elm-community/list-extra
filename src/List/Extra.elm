module List.Extra
    exposing
        ( last
        , init
        , getAt
        , (!!)
        , uncons
        , minimumBy
        , maximumBy
        , andMap
        , andThen
        , reverseMap
        , takeWhile
        , dropWhile
        , unique
        , uniqueBy
        , allDifferent
        , allDifferentBy
        , replaceIf
        , setAt
        , swapAt
        , stableSortWith
        , remove
        , updateIf
        , updateAt
        , updateIfIndex
        , removeAt
        , removeIfIndex
        , filterNot
        , iterate
        , initialize
        , cycle
        , intercalate
        , transpose
        , subsequences
        , permutations
        , interweave
        , cartesianProduct
        , foldl1
        , foldr1
        , indexedFoldl
        , indexedFoldr
        , scanl1
        , scanr
        , scanr1
        , mapAccuml
        , mapAccumr
        , unfoldr
        , splitAt
        , splitWhen
        , takeWhileRight
        , dropWhileRight
        , span
        , break
        , stripPrefix
        , group
        , groupWhile
        , groupWhileTransitively
        , inits
        , tails
        , select
        , selectSplit
        , isPrefixOf
        , isSuffixOf
        , isInfixOf
        , isSubsequenceOf
        , isPermutationOf
        , notMember
        , find
        , elemIndex
        , elemIndices
        , findIndex
        , findIndices
        , count
        , zip
        , zip3
        , zip4
        , zip5
        , lift2
        , lift3
        , lift4
        , groupsOf
        , groupsOfWithStep
        , greedyGroupsOf
        , greedyGroupsOfWithStep
        , groupsOfVarying
        )

{-| Convenience functions for working with List


# Basics

@docs last, init, getAt, (!!), uncons, maximumBy, minimumBy, andMap, andThen, reverseMap, takeWhile, dropWhile, unique, uniqueBy, allDifferent, allDifferentBy, replaceIf, setAt, remove, updateIf, updateAt, updateIfIndex, removeAt, removeIfIndex, filterNot, swapAt, stableSortWith


# List transformations

@docs intercalate, transpose, subsequences, permutations, interweave, cartesianProduct


# Folds

@docs foldl1, foldr1, indexedFoldl, indexedFoldr


# Building lists

@docs scanl1, scanr, scanr1, mapAccuml, mapAccumr, unfoldr, iterate, initialize, cycle


# Sublists

@docs splitAt, splitWhen, takeWhileRight, dropWhileRight, span, break, stripPrefix, group, groupWhile, groupWhileTransitively, inits, tails, select, selectSplit


# Predicates

@docs isPrefixOf, isSuffixOf, isInfixOf, isSubsequenceOf, isPermutationOf


# Searching

@docs notMember, find, elemIndex, elemIndices, findIndex, findIndices, count


# Zipping

@docs zip, zip3, zip4, zip5


# Lift functions onto multiple lists of arguments

@docs lift2, lift3, lift4


# Split to groups of given size

@docs groupsOf, groupsOfWithStep, groupsOfVarying, greedyGroupsOf, greedyGroupsOfWithStep

-}

import List exposing (..)
import Set exposing (Set)
import Tuple exposing (first, second)


{-| Extract the last element of a list.

    last [1,2,3] == Just 3
    last [] == Nothing

-}
last : List a -> Maybe a
last items =
    case items of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


{-| Return all elements of the list except the last one.

    init [1,2,3] == Just [1,2]
    init [] == Nothing

-}
init : List a -> Maybe (List a)
init items =
    case items of
        [] ->
            Nothing

        nonEmptyList ->
            nonEmptyList
                |> List.reverse
                |> List.tail
                |> Maybe.map List.reverse


{-| Returns `Just` the element at the given index in the list,
or `Nothing` if the index is out of range.
-}
getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing
    else
        List.head <| List.drop idx xs


{-| Alias for getAt, but with the parameters flipped.
-}
(!!) : List a -> Int -> Maybe a
(!!) =
    flip getAt


{-| Returns a list of repeated applications of `f`. If `f` returns `Nothing`
the iteration will stop. If it returns `Just y` then `y` will be added to the
list and the iteration will continue with `f y`.

    collatz : Int -> Maybe Int
    collatz n =
        if n == 1 then
            Nothing
        else
            Just <|
                if n % 2 == 0 then
                    n / 2
                else
                    3 * n + 1

    iterate collatz 13 == [13,40,20,10,5,16,8,4,2,1]

-}
iterate : (a -> Maybe a) -> a -> List a
iterate f x =
    case f x of
        Just x_ ->
            x :: iterate f x_

        Nothing ->
            [ x ]


{-| Initialize a list of some length with some function.

`initialize n f` creates a list of length `n` with the element at index `i` initialized to the result of `f i`.

-}
initialize : Int -> (Int -> a) -> List a
initialize n f =
    let
        step i acc =
            if i < 0 then
                acc
            else
                step (i - 1) (f i :: acc)
    in
        step (n - 1) []


{-| Creates a list of the given length whose elements are obtained by cycling
through the elements of the given list. If the given list is empty, the
resulting list will be empty.

    cycle 6 [ 4, 7, 8 ] == [ 4, 7, 8, 4, 7, 8 ]
    cycle 4 [ 'a', 'b', 'c' ] == [ 'a', 'b', 'c', 'a' ]
    cycle 9001 [] == []
    cycle 2 [ 1, 2, 3, 4, 5 ] == [ 1, 2 ]

-}
cycle : Int -> List a -> List a
cycle len list =
    let
        cycleLength =
            List.length list
    in
        if cycleLength == 0 || cycleLength == len then
            list
        else if cycleLength < len then
            List.reverse
                (reverseAppend
                    (List.take (rem len cycleLength) list)
                    (cycleHelp [] (len // cycleLength) list)
                )
        else
            List.take len list


cycleHelp : List a -> Int -> List a -> List a
cycleHelp acc n list =
    if n > 0 then
        cycleHelp (reverseAppend list acc) (n - 1) list
    else
        acc


{-| Decompose a list into its head and tail. If the list is empty, return `Nothing`. Otherwise, return `Just (x, xs)`, where `x` is head and `xs` is tail.

    uncons [1,2,3] == Just (1, [2,3])
    uncons [] = Nothing

-}
uncons : List a -> Maybe ( a, List a )
uncons xs =
    case xs of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )


{-| Find the first maximum element in a list using a comparable transformation
-}
maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy f ls =
    let
        maxBy x ( y, fy ) =
            let
                fx =
                    f x
            in
                if fx > fy then
                    ( x, fx )
                else
                    ( y, fy )
    in
        case ls of
            [ l_ ] ->
                Just l_

            l_ :: ls_ ->
                Just <| first <| foldl maxBy ( l_, f l_ ) ls_

            _ ->
                Nothing


{-| Find the first minimum element in a list using a comparable transformation
-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
    let
        minBy x ( y, fy ) =
            let
                fx =
                    f x
            in
                if fx < fy then
                    ( x, fx )
                else
                    ( y, fy )
    in
        case ls of
            [ l_ ] ->
                Just l_

            l_ :: ls_ ->
                Just <| first <| foldl minBy ( l_, f l_ ) ls_

            _ ->
                Nothing


{-| Take elements in order as long as the predicate evaluates to `True`
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate =
    let
        takeWhileMemo memo list =
            case list of
                [] ->
                    List.reverse memo

                x :: xs ->
                    if (predicate x) then
                        takeWhileMemo (x :: memo) xs
                    else
                        List.reverse memo
    in
        takeWhileMemo []


{-| Drop elements in order as long as the predicate evaluates to `True`
-}
dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                dropWhile predicate xs
            else
                list


{-| Remove duplicate values, keeping the first instance of each element which appears more than once.

    unique [0,1,1,0,1] == [0,1]

-}
unique : List comparable -> List comparable
unique list =
    uniqueHelp identity Set.empty list []


{-| Drop duplicates where what is considered to be a duplicate is the result of first applying the supplied function to the elements of the list.
-}
uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f list =
    uniqueHelp f Set.empty list []


{-| Indicate if list has duplicate values.

    allDifferent [0,1,1,0,1] == False

-}
allDifferent : List comparable -> Bool
allDifferent list =
    allDifferentBy identity list


{-| Indicate if list has duplicate values when supplied function are applyed on each values.
-}
allDifferentBy : (a -> comparable) -> List a -> Bool
allDifferentBy f list =
    List.length list == List.length (uniqueBy f list)


uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
                if Set.member computedFirst existing then
                    uniqueHelp f existing rest accumulator
                else
                    uniqueHelp f (Set.insert computedFirst existing) rest (first :: accumulator)


{-| Map functions taking multiple arguments over multiple lists. Each list should be of the same length.

    ((\a b c -> a + b * c)
        |> flip map [1,2,3]
        |> andMap [4,5,6]
        |> andMap [2,1,1]
    ) == [9,7,9]

-}
andMap : List a -> List (a -> b) -> List b
andMap l fl =
    map2 (<|) fl l


{-| Equivalent to `concatMap`. For example, suppose you want to have a cartesian product of [1,2] and [3,4]:

    [1,2] |> andThen (\x -> [3,4]
          |> andThen (\y -> [(x,y)]))

will give back the list:

    [(1,3),(1,4),(2,3),(2,4)]

Now suppose we want to have a cartesian product between the first list and the second list and its doubles:

    [1,2] |> andThen (\x -> [3,4]
          |> andThen (\y -> [y,y*2]
          |> andThen (\z -> [(x,z)])))

will give back the list:

    [(1,3),(1,6),(1,4),(1,8),(2,3),(2,6),(2,4),(2,8)]

Advanced functional programmers will recognize this as the implementation of bind operator (>>=) for lists from the `Monad` typeclass.

-}
andThen : (a -> List b) -> List a -> List b
andThen =
    concatMap


{-| `reverseMap f xs` gives the same result as `List.reverse (List.map f xs)`,
but is tail-recursive and slightly more efficient.

    reverseMap sqrt [1,4,9] == [3,2,1]

-}
reverseMap : (a -> b) -> List a -> List b
reverseMap f xs =
    foldl (\x acc -> f x :: acc) [] xs


{-| Negation of `member`.

    notMember 1 [1,2,3] == False
    notMember 4 [1,2,3] == True

-}
notMember : a -> List a -> Bool
notMember x =
    not << member x


{-| Find the first element that satisfies a predicate and return
Just that element. If none match, return Nothing.

    find (\num -> num > 5) [2, 4, 6, 8] == Just 6

-}
find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest


{-| Return the index of the first occurrence of the element. Otherwise, return `Nothing`. Indexing starts from 0.

    elemIndex 1 [1,2,3] == Just 0
    elemIndex 4 [1,2,3] == Nothing
    elemIndex 1 [1,2,1] == Just 0

-}
elemIndex : a -> List a -> Maybe Int
elemIndex x =
    findIndex ((==) x)


{-| Return all indices of occurrences of the element. If element is not found, return empty list. Indexing starts from 0.

    elemIndices 1 [1,2,3] == [0]
    elemIndices 4 [1,2,3] == []
    elemIndices 1 [1,2,1] == [0,2]

-}
elemIndices : a -> List a -> List Int
elemIndices x =
    findIndices ((==) x)


{-| Take a predicate and a list, return the index of the first element that satisfies the predicate. Otherwise, return `Nothing`. Indexing starts from 0.

    findIndex isEven [1,2,3] == Just 1
    findIndex isEven [1,3,5] == Nothing
    findIndex isEven [1,2,4] == Just 1

-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index
            else
                findIndexHelp (index + 1) predicate xs


{-| Take a predicate and a list, return indices of all elements satisfying the predicate. Otherwise, return empty list. Indexing starts from 0.

    findIndices isEven [1,2,3] == [1]
    findIndices isEven [1,3,5] == []
    findIndices isEven [1,2,4] == [1,2]

-}
findIndices : (a -> Bool) -> List a -> List Int
findIndices predicate =
    let
        consIndexIf index x acc =
            if predicate x then
                index :: acc
            else
                acc
    in
        indexedFoldr consIndexIf []


{-| Returns the number of elements in a list that satisfy a given predicate.
Equivalent to `List.length (List.filter pred list)` but more efficient.

    count (\n -> n % 2 == 1) [1,2,3,4,5,6,7] == 4
    count ((==) "yeah") ["She","loves","you","yeah","yeah","yeah"] == 3

-}
count : (a -> Bool) -> List a -> Int
count predicate =
    List.foldl
        (\x acc ->
            if predicate x then
                acc + 1
            else
                acc
        )
        0


{-| Replace all values that satisfy a predicate with a replacement value.
-}
replaceIf : (a -> Bool) -> a -> List a -> List a
replaceIf predicate replacement list =
    updateIf predicate (always replacement) list


{-| Replace all values that satisfy a predicate by calling an update function.
-}
updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate update list =
    List.map
        (\item ->
            if predicate item then
                update item
            else
                item
        )
        list


{-| Replace a value at a specific index by calling an update function. Return the original list if the index is out of range.

    updateAt 0 ((+) 1) [ 1, 2, 3 ] == [ 2, 2, 3 ]

See also `updateIfIndex`.

-}
updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list
    else
        let
            head =
                List.take index list

            tail =
                List.drop index list
        in
            case tail of
                x :: xs ->
                    head ++ fn x :: xs

                _ ->
                    list


{-| Replace a value at an index that satisfies a predicate, by calling an update function.

    updateIfIndex ((==) 2) ((+) 1) [ 1, 2, 3 ] == [ 1, 2, 4 ]

See also `updateAt`.

-}
updateIfIndex : (Int -> Bool) -> (a -> a) -> List a -> List a
updateIfIndex predicate update list =
    List.indexedMap
        (\i x ->
            if predicate i then
                update x
            else
                x
        )
        list


{-| Remove the first occurrence of a value from a list.
-}
remove : a -> List a -> List a
remove x xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if x == y then
                ys
            else
                y :: remove x ys


{-| Set a value in a list by index. Return the original list if the index is out of range.

    setAt 0 42 [ 1, 2, 3 ] == [ 42, 2, 3 ]

-}
setAt : Int -> a -> List a -> List a
setAt index value =
    updateAt index (always value)


{-| Similar to List.sortWith, this sorts values with a custom comparison function.
Unlike List.sortWith, this sort is guaranteed to be a stable sort.
Note that List.sortWith is faster and is preferred if sort stability is not required.
-}
stableSortWith : (a -> a -> Basics.Order) -> List a -> List a
stableSortWith pred list =
    let
        listWithIndex =
            List.indexedMap (\i a -> ( a, i )) list

        predWithIndex ( a1, i1 ) ( a2, i2 ) =
            let
                result =
                    pred a1 a2
            in
                case result of
                    Basics.EQ ->
                        Basics.compare i1 i2

                    _ ->
                        result
    in
        List.sortWith predWithIndex listWithIndex |> List.map first


{-| Swap two values in a list by index. Return the original list if the index is out of range.
If the same index is supplied twice the operation has no effect.

    swapAt 1 2 [ 1, 2, 3 ] == [ 1, 3, 2 ]

-}
swapAt : Int -> Int -> List a -> List a
swapAt index1 index2 l =
    if index1 == index2 || index1 < 0 then
        l
    else if index1 > index2 then
        swapAt index2 index1 l
    else
        let
            ( part1, tail1 ) =
                splitAt index1 l

            ( head2, tail2 ) =
                splitAt (index2 - index1) tail1
        in
            case ( uncons head2, uncons tail2 ) of
                ( Just ( value1, part2 ), Just ( value2, part3 ) ) ->
                    List.concat [ part1, value2 :: part2, value1 :: part3 ]

                _ ->
                    l


{-| Remove the element at an index from a list. Return the original list if the index is out of range.

    removeAt 0 [ 1, 2, 3 ] == [ 2, 3 ]

See also `removeIfIndex`.

-}
removeAt : Int -> List a -> List a
removeAt index l =
    if index < 0 then
        l
    else
        let
            head =
                List.take index l

            tail =
                List.drop index l |> List.tail
        in
            case tail of
                Nothing ->
                    l

                Just t ->
                    List.append head t


{-| Remove an element at an index that satisfies a predicate.

    removeIfIndex ((==) 2) [ 1, 2, 3 ] == [ 1, 2 ]

See also `removeAt`.

-}
removeIfIndex : (Int -> Bool) -> List a -> List a
removeIfIndex predicate =
    indexedFoldr
        (\index item acc ->
            if predicate index then
                acc
            else
                item :: acc
        )
        []


{-| Take a predicate and a list, and return a list that contains elements which fails to satisfy the predicate.
This is equivalent to `List.filter (not << predicate) list`.

    filterNot isEven [1,2,3,4] == [1,3]

-}
filterNot : (a -> Bool) -> List a -> List a
filterNot pred list =
    List.filter (not << pred) list


{-| Take a list and a list of lists, insert that list between every list in the list of lists, concatenate the result. `intercalate xs xss` is equivalent to `concat (intersperse xs xss)`.

    intercalate [0,0] [[1,2],[3,4],[5,6]] == [1,2,0,0,3,4,0,0,5,6]

-}
intercalate : List a -> List (List a) -> List a
intercalate xs =
    concat << intersperse xs


{-| Transpose rows and columns of the list of lists.

    transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]

If some rows are shorter than the following rows, their elements are skipped:

    transpose [[10,11],[20],[],[30,31,32]] == [[10,20,30],[11,31],[32]]

-}
transpose : List (List a) -> List (List a)
transpose listOfLists =
    List.foldr (List.map2 (::)) (List.repeat (rowsLength listOfLists) []) listOfLists


rowsLength : List (List a) -> Int
rowsLength listOfLists =
    case listOfLists of
        [] ->
            0

        x :: _ ->
            List.length x


{-| Return the list of all subsequences of a list.

    subsequences [1,2,3] == [[],[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

-}
subsequences : List a -> List (List a)
subsequences xs =
    [] :: subsequencesNonEmpty xs


{-| Return the list of all subsequences of the argument, except for the empty list.

    subsequencesNonEmpty [1,2,3] == [[1],[2],[1,2],[3],[1,3],[2,3],[1,2,3]]

-}
subsequencesNonEmpty : List a -> List (List a)
subsequencesNonEmpty xs =
    case xs of
        [] ->
            []

        x :: xs ->
            let
                f ys r =
                    ys :: (x :: ys) :: r
            in
                [ x ] :: foldr f [] (subsequencesNonEmpty xs)


{-| Return the list of of all permutations of a list. The result is in lexicographic order.

    permutations [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]

-}
permutations : List a -> List (List a)
permutations xs_ =
    case xs_ of
        [] ->
            [ [] ]

        xs ->
            let
                f ( y, ys ) =
                    map ((::) y) (permutations ys)
            in
                concatMap f (select xs)


{-| Return a list that contains elements from the two provided, in alternate order.
If one list runs out of items, append the items from the remaining list.

    interweave [1,3] [2,4] == [1,2,3,4]
    interweave [1,3,5,7] [2,4] == [1,2,3,4,5,7]
    interweave [4,9,16] [2,3,5,7] == [4,2,9,3,16,5,7]

-}
interweave : List a -> List a -> List a
interweave =
    interweaveHelp []


interweaveHelp : List a -> List a -> List a -> List a
interweaveHelp acc list1 list2 =
    case ( list1, list2 ) of
        ( x :: xs, y :: ys ) ->
            interweaveHelp (y :: x :: acc) xs ys

        ( [], _ ) ->
            reverseAppend acc list2

        ( _, [] ) ->
            reverseAppend acc list1


{-| Return the cartesian product of a list of lists.
If one list is empty, the result is an empty list.
If the list of lists is empty, the result is an empty singleton.

    cartesianProduct [[1,2],[3,4,5],[6]] == [[1,3,6],[1,4,6],[1,5,6],[2,3,6],[2,4,6],[2,5,6]]
    cartesianProduct [[1,2]] == [[1],[2]]
    cartesianProduct [[1,2],[],[6]] == []
    cartesianProduct [[]] == []
    cartesianProduct [] == [[]]

-}
cartesianProduct : List (List a) -> List (List a)
cartesianProduct ll =
    case ll of
        [] ->
            [ [] ]

        xs :: xss ->
            lift2 (::) xs (cartesianProduct xss)


reverseAppend : List a -> List a -> List a
reverseAppend list1 list2 =
    List.foldl (::) list2 list1


{-| Variant of `foldl` that has no starting value argument and treats the head of the list as its starting value. If the list is empty, return `Nothing`.

    foldl1 max [1,2,3,2,1] == Just 3
    foldl1 max [] == Nothing
    foldl1 (-) [1,2,3] == Just -4

-}
foldl1 : (a -> a -> a) -> List a -> Maybe a
foldl1 f xs =
    let
        mf x m =
            Just
                (case m of
                    Nothing ->
                        x

                    Just y ->
                        f y x
                )
    in
        List.foldl mf Nothing xs


{-| Variant of `foldr` that has no starting value argument and treats the last element of the list as its starting value. If the list is empty, return `Nothing`.

    foldr1 min [1,2,3,2,1] == Just 1
    foldr1 min [] == Nothing
    foldr1 (-) [1,2,3] == Just 2

-}
foldr1 : (a -> a -> a) -> List a -> Maybe a
foldr1 f xs =
    let
        mf x m =
            Just
                (case m of
                    Nothing ->
                        x

                    Just y ->
                        f x y
                )
    in
        List.foldr mf Nothing xs


{-| Variant of `foldl` that passes the index of the current element to the step function. `indexedFoldl` is to `List.foldl` as `List.indexedMap` is to `List.map`.
-}
indexedFoldl : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldl func acc list =
    let
        step x ( i, acc ) =
            ( i + 1, func i x acc )
    in
        second (List.foldl step ( 0, acc ) list)


{-| Variant of `foldr` that passes the index of the current element to the step function. `indexedFoldr` is to `List.foldr` as `List.indexedMap` is to `List.map`.
-}
indexedFoldr : (Int -> a -> b -> b) -> b -> List a -> b
indexedFoldr func acc list =
    let
        step x ( i, acc ) =
            ( i - 1, func i x acc )
    in
        second (List.foldr step ( List.length list - 1, acc ) list)


{-| `scanl1` is a variant of `scanl` that has no starting value argument.

Compare:

    List.scanl (+) 0 [1,2,3] == [0,1,3,6]
    scanl1 (+) [1,2,3] == [1,3,6]

    List.scanl (-) 0 [1,2,3] == [0,1,1,2]
    scanl1 (-) [1,2,3] == [1,1,2]

    List.scanl (flip (-)) 0 [1,2,3] == [0,-1,-3,-6]
    scanl1 (flip (-)) [1,2,3] == [1,-1,-4]

-}
scanl1 : (a -> a -> a) -> List a -> List a
scanl1 f xs_ =
    case xs_ of
        [] ->
            []

        x :: xs ->
            scanl f x xs


{-| `scanr` is a right-to-left dual of `scanl`. Note that:

    head (scanr f z xs) == foldr f z xs

Examples:

    scanr (+) 0 [1,2,3] == [6,5,3,0]
    scanr (-) 0 [1,2,3] == [2,-1,3,0]

-}
scanr : (a -> b -> b) -> b -> List a -> List b
scanr f acc xs_ =
    case xs_ of
        [] ->
            [ acc ]

        x :: xs ->
            case scanr f acc xs of
                (q :: _) as qs ->
                    f x q :: qs

                [] ->
                    []


{-| `scanr1` is a variant of `scanr` that has no starting value argument.

    scanr1 (+) [1,2,3] == [6,5,3]
    scanr1 (-) [1,2,3] == [2,-1,3]
    scanr1 (flip (-)) [1,2,3] == [0,1,3]

-}
scanr1 : (a -> a -> a) -> List a -> List a
scanr1 f xs_ =
    case xs_ of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: xs ->
            case scanr1 f xs of
                (q :: _) as qs ->
                    f x q :: qs

                [] ->
                    []


{-| The mapAccuml function behaves like a combination of map and foldl; it applies a
function to each element of a list, passing an accumulating parameter from left to right,
and returning a final value of this accumulator together with the new list.

    mapAccuml f a0 [ x1, x2, x3 ] == ( a3, [ y1, y2, y3 ] )

    --        x1    x2    x3
    --        |     |     |
    --  a0 -- f --- f --- f -> a3
    --        |     |     |
    --        y1    y2    y3

Add a running total to a list of numbers:

    mapAccuml (\a x -> ( a + x, ( x, a + x ) )) 0 [ 2, 4, 8 ]
        == ( 14, [ ( 2, 2 ), ( 4, 6 ), ( 8, 14 ) ] )

Map number by multiplying with accumulated sum:

    mapAccuml (\a x -> ( a + x, a * x )) 5 [ 2, 4, 8 ]
        == ( 19, [ 10, 28, 88 ] )

-}
mapAccuml : (a -> b -> ( a, c )) -> a -> List b -> ( a, List c )
mapAccuml f acc0 list =
    let
        ( accFinal, generatedList ) =
            List.foldl
                (\x ( acc1, ys ) ->
                    let
                        ( acc2, y ) =
                            f acc1 x
                    in
                        ( acc2, y :: ys )
                )
                ( acc0, [] )
                list
    in
        ( accFinal, List.reverse generatedList )


{-| The mapAccumr function behaves like a combination of map and foldr; it applies a
function to each element of a list, passing an accumulating parameter from right to left,
and returning a final value of this accumulator together with the new list.

    mapAccumr f a0 [ x1, x2, x3 ] == ( a3, [ y1, y2, y3 ] )

    --        x1    x2    x3
    --        |     |     |
    --  a3 <- f --- f --- f -- a0
    --        |     |     |
    --        y1    y2    y3

Add a count of remaining elements:

    mapAccumr (\a x -> ( a + 1, ( x, a ) )) 0 [ 2, 4, 8 ]
        == ( 3, [ ( 2, 2 ), ( 4, 1 ), ( 8, 0 ) ] )

Map number by multiplying with right-to-left accumulated sum:

    mapAccumr (\a x -> ( a + x, a * x )) 5 [ 2, 4, 8 ]
        == ( 19, [ 34, 52, 40 ] )

-}
mapAccumr : (a -> b -> ( a, c )) -> a -> List b -> ( a, List c )
mapAccumr f acc0 list =
    List.foldr
        (\x ( acc1, ys ) ->
            let
                ( acc2, y ) =
                    f acc1 x
            in
                ( acc2, y :: ys )
        )
        ( acc0, [] )
        list


{-| The `unfoldr` function is "dual" to `foldr`. `foldr` reduces a list to a summary value, `unfoldr` builds a list from a seed. The function takes a function and a starting element. It applies the function to the element. If the result is `Just (a, b)`, `a` is accumulated and the function is applied to `b`. If the result is `Nothing`, the list accumulated so far is returned.

    unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 5 == [5,4,3,2,1]

-}
unfoldr : (b -> Maybe ( a, b )) -> b -> List a
unfoldr f seed =
    case f seed of
        Nothing ->
            []

        Just ( a, b ) ->
            a :: unfoldr f b


{-| Take a number and a list, return a tuple of lists, where first part is prefix of the list of length equal the number, and second part is the remainder of the list. `splitAt n xs` is equivalent to `(take n xs, drop n xs)`.

    splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
    splitAt 1 [1,2,3] == ([1],[2,3])
    splitAt 3 [1,2,3] == ([1,2,3],[])
    splitAt 4 [1,2,3] == ([1,2,3],[])
    splitAt 0 [1,2,3] == ([],[1,2,3])
    splitAt (-1) [1,2,3] == ([],[1,2,3])

-}
splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( take n xs, drop n xs )


{-| Attempts to split the list at the first element where the given predicate is true. If the predicate is not true for any elements in the list, return nothing. Otherwise, return the split list.

    splitWhen (\n -> n == 3) [1,2,3,4,5] == Just ([1,2],[3,4,5])
    splitWhen (\n -> n == 6) [1,2,3,4,5] == Nothing

-}
splitWhen : (a -> Bool) -> List a -> Maybe ( List a, List a )
splitWhen predicate list =
    findIndex predicate list
        |> Maybe.map (\i -> splitAt i list)


{-| Take elements from the right, while predicate still holds.

    takeWhileRight ((<)5) (range 1 10) == [6,7,8,9,10]

-}
takeWhileRight : (a -> Bool) -> List a -> List a
takeWhileRight p =
    let
        step x ( xs, free ) =
            if p x && free then
                ( x :: xs, True )
            else
                ( xs, False )
    in
        first << foldr step ( [], True )


{-| Drop elements from the right, while predicate still holds.

    dropWhileRight ((<)5) (range 1 10) == [1,2,3,4,5]

-}
dropWhileRight : (a -> Bool) -> List a -> List a
dropWhileRight p =
    foldr
        (\x xs ->
            if p x && isEmpty xs then
                []
            else
                x :: xs
        )
        []


{-| Take a predicate and a list, return a tuple. The first part of the tuple is the longest prefix of that list, for each element of which the predicate holds. The second part of the tuple is the remainder of the list. `span p xs` is equivalent to `(takeWhile p xs, dropWhile p xs)`.

    span ((>) 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
    span ((>) 5) [1,2,3] == ([1,2,3],[])
    span ((>) 0) [1,2,3] == ([],[1,2,3])

-}
span : (a -> Bool) -> List a -> ( List a, List a )
span p xs =
    ( takeWhile p xs, dropWhile p xs )


{-| Take a predicate and a list, return a tuple. The first part of the tuple is the longest prefix of that list, for each element of which the predicate *does not* hold. The second part of the tuple is the remainder of the list. `break p xs` is equivalent to `(takeWhile (not p) xs, dropWhile (not p) xs)`.

    break ((<) 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
    break ((>) 5) [1,2,3] == ([],[1,2,3])
    break ((<) 5) [1,2,3] == ([1,2,3],[])

-}
break : (a -> Bool) -> List a -> ( List a, List a )
break p =
    span (not << p)


{-| Drop the given prefix from the list. If the list doesn't start with that prefix, return `Nothing`.

    stripPrefix [1,2] [1,2,3,4] == Just [3,4]
    stripPrefix [1,2,3] [1,2,3,4,5] == Just [4,5]
    stripPrefix [1,2,3] [1,2,3] == Just []
    stripPrefix [1,2,3] [1,2] == Nothing
    stripPrefix [3,2,1] [1,2,3,4,5] == Nothing

-}
stripPrefix : List a -> List a -> Maybe (List a)
stripPrefix prefix xs =
    let
        step e m =
            case m of
                Nothing ->
                    Nothing

                Just [] ->
                    Nothing

                Just (x :: xs_) ->
                    if e == x then
                        Just xs_
                    else
                        Nothing
    in
        foldl step (Just xs) prefix


{-| Group similar elements together. `group` is equivalent to `groupWhile (==)`.

    group [1,2,2,3,3,3,2,2,1] == [[1],[2,2],[3,3,3],[2,2],[1]]

-}
group : List a -> List (List a)
group =
    groupWhile (==)


{-| Group elements together, using a custom equality test.

    groupWhile (\x y -> first x == first y) [(0,'a'),(0,'b'),(1,'c'),(1,'d')] == [[(0,'a'),(0,'b')],[(1,'c'),(1,'d')]]

The equality test should be an [equivalence relation](https://en.wikipedia.org/wiki/Equivalence_relation), i.e. it should have the properties:

  - Reflexivity - Testing an object against itself returns `True`.
  - Symmetry - Testing two objects should give the same result regardless of the order they are passed.
  - Transitivity - If the test on a first object and a second object results in `True`, and further if the test on that second object and a third also results in `True`, then the test should result in `True` when the first and third objects are passed.

For non-equivalent relations `groupWhile` has non-intuitive behavior. For example, inequality comparisons like `(<)` are not equivalence relations, so do *not* write `groupWhile (<) [1,3,5,2,4]`, as it will give an unexpected answer.

For grouping elements with a comparison test which is merely transitive, such as `(<)` or `(<=)`, see `groupWhileTransitively`.

-}
groupWhile : (a -> a -> Bool) -> List a -> List (List a)
groupWhile eq xs_ =
    case xs_ of
        [] ->
            []

        x :: xs ->
            let
                ( ys, zs ) =
                    span (eq x) xs
            in
                (x :: ys) :: groupWhile eq zs


{-| Group elements together, using a custom comparison test. Start a new group each time the comparison test doesn't hold for two adjacent elements.

    groupWhileTransitively (<) [1,2,3,2,4,1,3,2,1] == [[1,2,3],[2,4],[1,3],[2],[1]]

-}
groupWhileTransitively : (a -> a -> Bool) -> List a -> List (List a)
groupWhileTransitively compare list =
    groupWhileTransitivelyHelp [] [] compare list


groupWhileTransitivelyHelp : List (List a) -> List a -> (a -> a -> Bool) -> List a -> List (List a)
groupWhileTransitivelyHelp result currentGroup compare list =
    case list of
        [] ->
            List.reverse <|
                if List.isEmpty currentGroup then
                    result
                else
                    List.reverse (currentGroup :: result)

        [ x ] ->
            List.reverse <|
                (List.reverse (x :: currentGroup) :: result)

        first :: ((second :: _) as rest) ->
            if compare first second then
                groupWhileTransitivelyHelp
                    result
                    (first :: currentGroup)
                    compare
                    rest
            else
                groupWhileTransitivelyHelp
                    (List.reverse (first :: currentGroup) :: result)
                    []
                    compare
                    rest


{-| Return all initial segments of a list, from shortest to longest, empty list first, the list itself last.

    inits [1,2,3] == [[],[1],[1,2],[1,2,3]]

-}
inits : List a -> List (List a)
inits =
    foldr (\e acc -> [] :: map ((::) e) acc) [ [] ]


{-| Return all final segments of a list, from longest to shortest, the list itself first, empty list last.

    tails [1,2,3] == [[1,2,3],[2,3],[3],[]]

-}
tails : List a -> List (List a)
tails =
    foldr tailsHelp [ [] ]


tailsHelp : a -> List (List a) -> List (List a)
tailsHelp e list =
    case list of
        x :: xs ->
            (e :: x) :: x :: xs

        [] ->
            []


{-| Return all combinations in the form of (element, rest of the list). Read [Haskell Libraries proposal](https://mail.haskell.org/pipermail/libraries/2008-February/009270.html) for further ideas on how to use this function.

    select [1,2,3,4] == [(1,[2,3,4]),(2,[1,3,4]),(3,[1,2,4]),(4,[1,2,3])]

-}
select : List a -> List ( a, List a )
select xs =
    case xs of
        [] ->
            []

        x :: xs ->
            ( x, xs ) :: map (\( y, ys ) -> ( y, x :: ys )) (select xs)


{-| Return all combinations in the form of (elements before, element, elements after).

    selectSplit [1,2,3] == [([],1,[2,3]),([1],2,[3]),([1,2],3,[])]

-}
selectSplit : List a -> List ( List a, a, List a )
selectSplit xs =
    case xs of
        [] ->
            []

        x :: xs ->
            ( [], x, xs ) :: map (\( lys, y, rys ) -> ( x :: lys, y, rys )) (selectSplit xs)


{-| Take two lists and return `True`, if the first list is the prefix of the second list.
-}
isPrefixOf : List a -> List a -> Bool
isPrefixOf prefix xs =
    case ( prefix, xs ) of
        ( [], _ ) ->
            True

        ( _ :: _, [] ) ->
            False

        ( p :: ps, x :: xs ) ->
            p == x && isPrefixOf ps xs


{-| Take two lists and return `True`, if the first list is the suffix of the second list.
-}
isSuffixOf : List a -> List a -> Bool
isSuffixOf suffix xs =
    isPrefixOf (reverse suffix) (reverse xs)


{-| Return True if all the elements of the first list occur in-order and
consecutively anywhere within the second.

    isInfixOf [5, 7, 11] [2, 3, 5, 7, 11, 13] == True
    isInfixOf [5, 7, 13] [2, 3, 5, 7, 11, 13] == False
    isInfixOf [3, 5, 2] [2, 3, 5, 7, 11, 13] == False

-}
isInfixOf : List a -> List a -> Bool
isInfixOf infixList list =
    case infixList of
        [] ->
            True

        x :: xs ->
            isInfixOfHelp x xs list


isInfixOfHelp : a -> List a -> List a -> Bool
isInfixOfHelp infixHead infixTail list =
    case list of
        [] ->
            False

        x :: xs ->
            if x == infixHead then
                isPrefixOf infixTail xs
            else
                isInfixOfHelp infixHead infixTail xs


{-| Return True if all the elements of the first list occur, in order, in the
second. The elements do not have to occur consecutively.

    isSubsequenceOf ["E", "l", "m"] ["E", "a", "t", " ", "l", "i", "m", "e", "s"] == True
    isSubsequenceOf ["E", "l", "m"] ["E", "m", "a", "i", "l"] == False

-}
isSubsequenceOf : List a -> List a -> Bool
isSubsequenceOf subseq list =
    case ( subseq, list ) of
        ( [], _ ) ->
            True

        ( _, [] ) ->
            False

        ( x :: xs, y :: ys ) ->
            if x == y then
                isSubsequenceOf xs ys
            else
                isSubsequenceOf subseq ys


{-| Take two lists and return `True`, if the first list is a permutation of the second list.
-}
isPermutationOf : List a -> List a -> Bool
isPermutationOf permut xs =
    member permut (permutations xs)


{-| Take two lists and returns a list of corresponding pairs
-}
zip : List a -> List b -> List ( a, b )
zip =
    map2 (,)


{-| Take three lists and returns a list of triples
-}
zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 =
    map3 (,,)


{-| Take four lists and returns a list of quadruples
-}
zip4 : List a -> List b -> List c -> List d -> List ( a, b, c, d )
zip4 =
    map4 (,,,)


{-| Take five lists and returns a list of quintuples
-}
zip5 : List a -> List b -> List c -> List d -> List e -> List ( a, b, c, d, e )
zip5 =
    map5 (,,,,)


{-| Map functions taking multiple arguments over multiple lists, regardless of list length.
All possible combinations will be explored.

lift2 (+) [1,2,3][4,5] == [5,6,6,7,7,8]

-}
lift2 : (a -> b -> c) -> List a -> List b -> List c
lift2 f la lb =
    la |> andThen (\a -> lb |> andThen (\b -> [ f a b ]))


{-| -}
lift3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
lift3 f la lb lc =
    la |> andThen (\a -> lb |> andThen (\b -> lc |> andThen (\c -> [ f a b c ])))


{-| -}
lift4 : (a -> b -> c -> d -> e) -> List a -> List b -> List c -> List d -> List e
lift4 f la lb lc ld =
    la |> andThen (\a -> lb |> andThen (\b -> lc |> andThen (\c -> ld |> andThen (\d -> [ f a b c d ]))))


{-| Split list into groups of length `size`. If there are not enough elements
to completely fill the last group, it will not be included. This is equivalent
to calling `groupsOfWithStep` with the same `size` and `step`.

    groupsOf 3 (range 1 10) == [[1,2,3],[4,5,6],[7,8,9]]

-}
groupsOf : Int -> List a -> List (List a)
groupsOf size xs =
    groupsOfWithStep size size xs


{-| Split list into groups of length `size` at offsets `step` apart. If there
are not enough elements to completely fill the last group, it will not be
included. (See `greedyGroupsOfWithStep` if you would like the last group to be
included regardless.)

    groupsOfWithStep 4 4 (range 1 10) == [[1,2,3,4],[5,6,7,8]]
    groupsOfWithStep 3 1 (range 1 5) == [[1,2,3],[2,3,4],[3,4,5]]
    groupsOfWithStep 3 6 (range 1 20) == [[1,2,3],[7,8,9],[13,14,15]]

If `step == size`, every element (except for perhaps the last few due to the
non-greedy behavior) will appear in exactly one group. If `step < size`, there
will be an overlap between groups. If `step > size`, some elements will be
skipped and not appear in any groups.

-}
groupsOfWithStep : Int -> Int -> List a -> List (List a)
groupsOfWithStep size step xs =
    let
        group =
            List.take size xs

        xs_ =
            List.drop step xs

        okayArgs =
            size > 0 && step > 0

        okayLength =
            size == List.length group
    in
        if okayArgs && okayLength then
            group :: groupsOfWithStep size step xs_
        else
            []


{-| `groupsOfVarying ns` takes `n` elements from a list for each `n` in `ns`, splitting the list into variably sized segments

    groupsOfVarying [2, 3, 1] ["a", "b", "c", "d", "e", "f"] == [["a", "b"], ["c", "d", "e"], ["f"]]
    groupsOfVarying [2] ["a", "b", "c", "d", "e", "f"] == [["a", "b"]]
    groupsOfVarying [2, 3, 1, 5, 6] ["a", "b", "c", "d", "e"] == [["a", "b"], ["c", "d", "e"]]

-}
groupsOfVarying : List Int -> List a -> List (List a)
groupsOfVarying listOflengths list =
    groupsOfVarying_ listOflengths list []


groupsOfVarying_ : List Int -> List a -> List (List a) -> List (List a)
groupsOfVarying_ listOflengths list accu =
    case ( listOflengths, list ) of
        ( length :: tailLengths, _ :: _ ) ->
            let
                ( head, tail ) =
                    splitAt length list
            in
                groupsOfVarying_ tailLengths tail (head :: accu)

        _ ->
            List.reverse accu


{-| Greedily split list into groups of length `size`. The last group of
elements will be included regardless of whether there are enough elements in
the list to completely fill it. This is equivalent to calling
`greedyGroupsOfWithStep` with the same `size` and `step`.

    greedyGroupsOf 3 (range 1 10) == [[1,2,3],[4,5,6],[7,8,9],[10]]

-}
greedyGroupsOf : Int -> List a -> List (List a)
greedyGroupsOf size xs =
    greedyGroupsOfWithStep size size xs


{-| Greedily split list into groups of length `size` at offsets `step` apart.
The last group of elements will be included regardless of whether there are
enough elements in the list to completely fill it. (See `groupsOfWithStep`
for the non-greedy version of this function).

    greedyGroupsOfWithStep 4 4 (range 1 10) == [[1,2,3,4],[5,6,7,8],[9,10]]
    greedyGroupsOfWithStep 3 2 (range 1 6) == [[1,2,3],[3,4,5],[5,6]]
    greedyGroupsOfWithStep 3 6 (range 1 20) == [[1,2,3],[7,8,9],[13,14,15],[19,20]]

If `step == size`, every element will appear in exactly one group. If
`step < size`, there will be an overlap between groups. If `step > size`, some
elements will be skipped and not appear in any groups.

-}
greedyGroupsOfWithStep : Int -> Int -> List a -> List (List a)
greedyGroupsOfWithStep size step xs =
    let
        group =
            List.take size xs

        xs_ =
            List.drop step xs

        okayArgs =
            size > 0 && step > 0

        okayXs =
            List.length xs > 0
    in
        if okayArgs && okayXs then
            group :: greedyGroupsOfWithStep size step xs_
        else
            []
