module GraphEditor.Util where

import List as L

intercalate : List a -> List (List a) -> List a
intercalate sep xs = L.concat <| L.intersperse sep xs

startsWith : List a -> List a -> Bool
startsWith a prefix =
    case (a, prefix) of
      ([], []) -> True
      (xs, []) -> True
      ([], xs) -> False
      (x::xs, y::ys) -> if x == y then startsWith xs ys else False
