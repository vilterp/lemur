module Codegen.TestData where

import Codegen.AST (..)

fib =
  FuncDef
    { name = "fib"
    , args = ["n"]
    , body =
      [ IfStmt
        { cond =
            FuncCall
              { func = MemberAccess (Variable "operator") "or_"
              , args =
                [ FuncCall
                    { func = MemberAccess (Variable "operator") "eq"
                    , args =
                      [ Variable "n"
                      , IntLit 0
                      ]
                    }
                , FuncCall
                    { func = MemberAccess (Variable "operator") "eq"
                    , args =
                      [ Variable "n"
                      , IntLit 1
                      ]
                    }
                ]
              }
        , ifBlock = [Return <| IntLit 1]
        , elseBlock =
          [ Return <|
              FuncCall
                { func = MemberAccess (Variable "operator") "add"
                , args =
                  [ FuncCall
                      { func = MemberAccess (Variable "operator") "sub"
                      , args =
                        [ Variable "n"
                        , IntLit 1
                        ]
                      }
                  , FuncCall
                      { func = MemberAccess (Variable "operator") "sub"
                      , args =
                        [ Variable "n"
                        , IntLit 2
                        ]
                      }
                  ]
                }
          ]
        }
      ]
    }
