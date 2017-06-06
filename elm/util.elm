module Util exposing (pairs)


-- Given two integers, construct the list of their permutations.
pairs : Int -> Int -> List (Int, Int)
pairs from to =
  let
    range = (List.range from to)
  in
    List.concat (List.map (\r -> (List.map (\c -> (r, c)) range)) range)
