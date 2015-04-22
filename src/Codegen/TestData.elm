module Codegen.TestData where

import Codegen.AST exposing (..)

fib =
  FuncDef
    { name = "fib"
    , args = ["n"]
    , body =
      [ IfStmt
        { cond =
            BinOp
              "or"
              (BinOp
                "=="
                (Variable "n")
                (IntLit 0))
              (BinOp
                "=="
                (Variable "n")
                (IntLit 1))
        , ifBlock = [Return <| IntLit 1]
        , elseBlock =
          [ Return <|
              BinOp
                "+"
                (FuncCall
                  { func = Variable "fib"
                  , args =
                    [ (BinOp
                        "-"
                        (Variable "n")
                        (IntLit 1))
                    ]
                  })
                (FuncCall
                  { func = Variable "fib"
                  , args =
                    [ (BinOp
                        "-"
                        (Variable "n")
                        (IntLit 1))
                    ]
                  })
          ]
        }
      ]
    }
