module List.Extra (minimumBy, maximumBy) where
{-| Convenience functions for working with List

# Common Helpers
@docs maximumBy, minimumBy

-}

import List (..)

{-| Find the first maximum element in a list using a comparable transformation
-}
maximumBy : (a -> comparable) -> List a -> a
maximumBy f =
  let maxBy f x y = if (f x) > (f y) then x else y
  in foldl1 <| maxBy f

{-| Find the first minimum element in a list using a comparable transformation
-}
minimumBy : (a -> comparable) -> List a -> a
minimumBy f =
  let minBy f x y = if (f x) < (f y) then x else y
  in foldl1 <| minBy f
