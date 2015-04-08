module UnionFind where

-- dumb implementation cuz I don't have mutable references

import Dict as D
import Set as S
import Maybe as M

type alias UnionFind comparable = D.Dict comparable (S.Set comparable)

empty : UnionFind comparable
empty = D.empty

makeSet : comparable -> UnionFind comparable -> UnionFind comparable
makeSet item uf =
    D.insert item (S.fromList [item]) uf

getRepr : comparable -> UnionFind comparable -> Maybe (comparable, S.Set comparable)
getRepr x uf =
    M.oneOf <| D.values <| D.map (\k v -> if x `S.member` v then Just (k, v) else Nothing) uf

-- could really uze dat `do` notation here
union : comparable -> comparable -> UnionFind comparable -> Maybe (UnionFind comparable)
union x y uf =
    let xRes = getRepr x uf
        yRes = getRepr y uf
    in case (xRes, yRes) of
        (Just (xRoot, xItems), Just (yRoot, yItems)) -> Just
          (if xRoot == yRoot
           then uf -- already unified
           else D.remove yRoot uf
                |> D.remove xRoot
                |> D.insert xRoot (xItems `S.union` yItems))
        _ -> Nothing

sameSet : comparable -> comparable -> UnionFind comparable -> Bool
sameSet x y uf = (getRepr x uf) == (getRepr y uf) -- hope this doesn't actually do set equality
-- would be nice to return something more descriptive than `False` if one or both
-- weren't found, but oh well

-- TODO: get this to typecheck. it gives you a result telling you which variable
-- wasn't found in error case
--union uf x y =
--    (R.fromMaybe ("not found: " ++ toString x) <| getRepr uf x)
--      `R.andThen` (\(xRoot, xItems) -> (R.fromMaybe ("not found: " ++ toString y) <| getRepr uf y)
--        `R.map` (\(yRoot, yItems) ->
--          if xRoot == yRoot
--          then uf -- already unified
--          else D.remove yRoot uf
--                |> D.remove xRoot
--                |> D.insert xRoot (xItems `S.union` yItems)))