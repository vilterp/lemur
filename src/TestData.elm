module TestData where

import Model exposing (..)

import Dict as D
import List as L
import Result as R

import Debug

import Codegen

helloMap : Module
helloMap =
    { name = "HelloMap"
    , builtinFuncs =
        D.fromList
            [ ( "names"
              , BuiltinFunc { name = "names"
                            , params = []
                            , returnVals = ["names"]
                            , pythonCode = "return {'names':['Pete', 'Borja', 'Ravi', 'Mike']}"
                            }
              )
            , ( "Map"
              , BuiltinFunc { name = "Map"
                            , params = ["function", "list"]
                            , returnVals = ["mappedList"]
                            , pythonCode = "def apply_single(item):\n  return function(item).values()[0]\nreturn {'mappedList': map(apply_single, list)}"
                            }
              )
            , ( "add_excl"
              , BuiltinFunc { name = "add_excl"
                            , params = ["str"]
                            , returnVals = ["added"]
                            , pythonCode = "return {'added':str + '!'}"
                            }
              )
            , ( "add_hi"
              , BuiltinFunc { name = "add_hi"
                            , params = ["str"]
                            , returnVals = ["added"]
                            , pythonCode = "return {'added': 'Hi ' + str}"
                            }
              )
            -- TODO: define this in terms of join;
            -- define join in terms of fold
            , ( "newline_join"
              , BuiltinFunc { name = "newline_join"
                            , params = ["list"]
                            , returnVals = ["joinedList"]
                            , pythonCode = "return {'joinedList': '\\n'.join(list)}"
                            }
              )
            ]
    , userFuncs =
        D.fromList
          [ ("main"
            , UserFunc { name = "main"
                       , graph = mainGraph
                       , nextApId = 4
                       , nextLambdaId = 1
                       }
            )
          , ("exclAndHi"
            , UserFunc { name = "exclAndHi"
                       , graph = exclAndHiGraph
                       , nextApId = 3
                       , nextLambdaId = 0
                       }
            )
          ]
    }

-- real node ids will just be like "ap4" -- function names
-- are just for readability of this example

mainGraph : Graph
mainGraph =
    { nodes =
        D.fromList
            [ ("ap0_names", { pos = (-200, -200)
                            , id = "ap0_names"
                            , node = ApNode "names"
                            })
            , ("ap1_exclAndHi", { pos = (-200, -200)
                              , id = "ap1_exclAndHi"
                              , node = ApNode "exclAndHi"
                              })
            , ("ap3_map", { pos = (100, 0)
                          , id = "ap3_map"
                          , node = ApNode "Map"
                          })
            , ("ap4_newline_join", { pos = (300, 0)
                                   , id = "ap4_newline_join"
                                   , node = ApNode "newline_join"
                                   })
            ]
    , edges =
        [ { from = (["ap0_names"], ApResultSlot "names")
          , to = (["ap3_map"], ApParamSlot "list")
          }
        , { from = (["ap1_exclAndHi"], FuncValueSlot)
          , to = (["ap3_map"], ApParamSlot "function")
          }
        , { from = (["ap3_map"], ApResultSlot "mappedList")
          , to = (["ap4_newline_join"], ApParamSlot "list")
          }
        ]
    }

exclAndHiGraph : Graph
exclAndHiGraph =
    { nodes =
        D.fromList
          [ ("ap1_add_excl", { pos = (-100, 0)
                             , id = "ap1_add_excl"
                             , node = ApNode "add_excl"
                             })
          , ("ap2_add_hi", { pos = (100, 0)
                           , id = "ap2_add_hi"
                           , node = ApNode "add_hi"
                           })
          ]
    , edges =
        [ { from = (["ap1_add_excl"], ApResultSlot "added")
          , to = (["ap2_add_hi"], ApParamSlot "str")
          }
        ]
    }

code = Codegen.moduleToPython helloMap
