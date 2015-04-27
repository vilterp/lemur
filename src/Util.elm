module Util where

import List as L
import Set as S

import Debug

intercalate : List a -> List (List a) -> List a
intercalate sep xs = L.concat <| L.intersperse sep xs

startsWith : List a -> List a -> Bool
startsWith a prefix =
    case (a, prefix) of
      ([], []) -> True
      (xs, []) -> True
      ([], xs) -> False
      (x::xs, y::ys) -> if x == y then startsWith xs ys else False

multiUnion : List (S.Set comparable) -> S.Set comparable
multiUnion sets =
    L.foldl (\s union -> s `S.union` union) S.empty sets

getOrCrash : Result String r -> r
getOrCrash res =
    case res of
      Err e -> Debug.crash e
      Ok r -> r

getMaybeOrCrash : String -> Maybe a -> a
getMaybeOrCrash msg m =
    case m of
      Just x -> x
      Nothing -> Debug.crash msg
