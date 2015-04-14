module Codegen.PrettyPrint where

import String as S
import List as L

type IndentTree = IndentTree String (List IndentTree)

leaf : String -> IndentTree
leaf str = IndentTree str []

headerBlock : String -> List (IndentTree) -> IndentTree
headerBlock = IndentTree

stringify : IndentTree -> String
stringify tree =
    let indent lvl = S.repeat lvl "  "
        recurse indentLevel (IndentTree txt children) =
          ((indent indentLevel) ++ txt) :: (L.map (recurse (indentLevel + 1)) children) |>
            S.join "\n"
    in recurse 0 tree
