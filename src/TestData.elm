module TestData where

import Model exposing (..)

import Dict as D
import List as L
import Result as R

import Debug


helloMap : Module
helloMap =
    { name = "HelloMap"
    , builtinFuncs =
        D.fromList
            [ ( "names"
              , BuiltinFunc { name = "names"
                            , params = []
                            , returnVals = ["names"]
                            , pythonCode = "return ['Pete', 'Borja', 'Ravi', 'Mike']"
                            }
              )
            , ( "map"
              , BuiltinFunc { name = "map"
                            , params = ["function", "list"]
                            , returnVals = ["mappedList"]
                            , pythonCode = "return map(function, list)"
                            }
              )
            , ( "addExcl"
              , BuiltinFunc { name = "addExcl"
                            , params = ["str"]
                            , returnVals = ["added"]
                            , pythonCode = "return str + '!'"
                            }
              )
            , ( "addHi"
              , BuiltinFunc { name = "addHi"
                            , params = ["str"]
                            , returnVals = ["added"]
                            , pythonCode = "return 'Hi ' + str"
                            }
              )
            -- TODO: define this in terms of join;
            -- define join in terms of fold
            , ( "newlineJoin"
              , BuiltinFunc { name = "newlineJoin"
                            , params = ["list"]
                            , returnVals = ["joinedList"]
                            , pythonCode = "return '\\n'.join(list)"
                            }
              )
            ]
    , userFuncs =
        D.fromList
          [ ("main"
            , UserFunc { name = "main"
                       , graph = testGraph
                       , params = []
                       , returnVals = ["result"]
                       , nextApId = 4
                       , nextLambdaId = 1
                       }
            )
          ]
    }

testGraph : Graph
testGraph =
    { nodes =
        D.fromList
            [ ("ap0-names", { pos = (-200, -200)
                            , id = "ap0-names"
                            , node = ApNode "names"
                            })
            , ("lambda0"
              , { pos = (-300, 300)
                , id = "lambda0"
                , node = LambdaNode
                    { nodes =
                        D.fromList
                          [ ("ap1-addExcl", { pos = (-100, 0)
                                            , id = "ap1-addExcl"
                                            , node = ApNode "addExcl"
                                            })
                          , ("ap2-addHi", { pos = (100, 0)
                                          , id = "ap2-addHi"
                                          , node = ApNode "addHi"
                                          })
                          ]
                    , dims = { width = 300, height = 200 }
                    }
                }
              )
            , ("ap3-map", { pos = (100, 0)
                          , id = "ap3-map"
                          , node = ApNode "map"
                          })
            , ("ap4-newlineJoin", { pos = (300, 0)
                                  , id = "ap4-newlineJoin"
                                  , node = ApNode "newlineJoin"
                                  })
            ]
    , edges =
        [ { from = (["ap0-names"], ApResultSlot "names")
          , to = (["ap3-map"], ApParamSlot "list")
          }
        , { from = (["lambda0"], FuncValueSlot)
          , to = (["ap3-map"], ApParamSlot "function")
          }
        , { from = (["ap3-map"], ApResultSlot "mappedList")
          , to = (["ap4-newlineJoin"], ApParamSlot "list")
          }
        , { from = (["lambda0", "ap1-addExcl"], ApResultSlot "added")
          , to = (["lambda0", "ap2-addHi"], ApParamSlot "str")
          }
        ]
    }
