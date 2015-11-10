module Utils where

import List
import Random


partitionByN : Int -> List a -> List (List a)
partitionByN n list =
  if List.isEmpty list then [] else
    let
      catch = (List.take n list)
    in
      if n == (List.length catch) then
        [catch] ++ (partitionByN n (List.drop n list))
      else
        [catch]


randBools : Int -> Int -> List Bool
randBools count seedVal =
    let
      intList =
        Random.generate (Random.list count <| Random.int 0 1000) (Random.initialSeed seedVal)
        |> fst
    in
      List.map (\n -> (n % 4) == 1 ) intList
