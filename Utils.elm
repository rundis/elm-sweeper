module Utils where

import List

--partitionByN : Int -> [a] -> [[a]]

partitionByN n list =
  if List.isEmpty list then [] else
    let
      catch = (List.take n list)
    in
      if n == (List.length catch) then
        [catch] ++ (partitionByN n (List.drop n list))
      else
        [catch]
