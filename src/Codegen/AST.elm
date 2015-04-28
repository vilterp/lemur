module Codegen.AST where

import Dict as D
import List as L
import String as S

import Codegen.PrettyPrint exposing (..)

type Expr
    = IntLit Int
    | StringLit String
    | Variable String
    | MemberAccess Expr String
    | DictAccess Expr String
    | FuncCall { func : Expr, args : List Expr }
    | BinOp String Expr Expr
    | DictLiteral (D.Dict String Expr)
    | None

type Statement
    = FuncDef { name : String, args : List String, body : List Statement }
    | VarAssn { varName : String, expr : Expr }
    | IfStmt { cond : Expr, ifBlock : List Statement, elseBlock : List Statement }
    | Return Expr

parens : String -> String
parens str = "(" ++ str ++ ")"

-- I feel like there are a lot of little bugs lurking in here
-- with parenthesization, escaping, etc
exprToPython : Expr -> String
exprToPython expr =
    case expr of
      IntLit x -> toString x
      StringLit str -> toString str
      Variable str -> str
      MemberAccess exp memb ->
          (exprToPython exp) ++ "." ++ memb
      DictAccess exp name ->
          (exprToPython exp) ++ "[" ++ (toString name) ++ "]"
      FuncCall {func, args} ->
          (exprToPython func) ++
            (parens <|
              S.join ", " <|
                L.map exprToPython args)
      DictLiteral dict ->
          let items = D.toList dict
                |> L.map (\(k, v) -> (toString k) ++ ": " ++ (exprToPython v))
                |> S.join ", "
          in "{" ++ items ++ "}"
      BinOp op left right ->
          "(" ++
            (exprToPython left) ++
              " " ++ op ++ " " ++
                (exprToPython right) ++ ")"
      None -> "None"

statementToPython : Statement -> String
statementToPython stmt =
    let recurse stmt =
          case stmt of
            FuncDef {name, args, body} ->
                [ headerBlock
                    ("def " ++ name ++ (parens <| S.join ", " args) ++ ":")
                    (blockToTree body)
                ]
            IfStmt {cond, ifBlock, elseBlock} ->
                [ headerBlock
                    ("if " ++ (exprToPython cond) ++ ":")
                    (blockToTree ifBlock)
                , headerBlock
                    "else:"
                    (blockToTree elseBlock)
                ]
            VarAssn {varName, expr} ->
                [leaf <| varName ++ " = " ++ (exprToPython expr)]
            Return expr ->
                [leaf <| "return " ++ (exprToPython expr)]
        blockToTree block =
          case block of
            [] -> [ leaf "pass" ]
            _ -> L.concatMap recurse block
    in recurse stmt |> L.map stringify |> S.join "\n\n"
