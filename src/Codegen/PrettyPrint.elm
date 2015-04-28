module Codegen.PrettyPrint where

import String as S
import List as L

type IndentTree = IndentTree String (List IndentTree)
                | PreIndentedBlock String -- assume 2 spaces indentation!

leaf : String -> IndentTree
leaf str = IndentTree str []

headerBlock : String -> List (IndentTree) -> IndentTree
headerBlock = IndentTree

preIndented : String -> IndentTree
preIndented = PreIndentedBlock

stringify : IndentTree -> String
stringify tree =
    let indent lvl = S.repeat lvl "  "
        recurse indentLevel tree =
            case tree of
              IndentTree txt children ->
                  ((indent indentLevel) ++ txt) :: (L.map (recurse (indentLevel + 1)) children)
                    |> S.join "\n"
              PreIndentedBlock text ->
                  S.split "\n" text
                    |> L.map (\line -> (indent indentLevel) ++ line)
                    |> S.join "\n"
    in recurse 0 tree
