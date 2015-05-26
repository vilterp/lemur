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
                            , pythonCode = "arg_name = function.func_code.co_varnames[0]\ndef apply_single(item):\n  result = log_call(function, 'map_internal', {arg_name: item})\n  return result.values()[0]\nreturn {'mappedList': map(apply_single, list)}"
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
            , ( "join"
              , BuiltinFunc { name = "join"
                            , params = ["joiner", "list"]
                            , returnVals = ["joinedList"]
                            , pythonCode = "return {'joinedList': joiner.join(list)}"
                            }
              )
            , ( "Zero" 
              , BuiltinFunc { name = "Zero"
                            , params = []
                            , returnVals = ["num"]
                            , pythonCode = "return {'num': 0}"
                            }
              )
            , ( "One" 
              , BuiltinFunc { name = "One"
                            , params = []
                            , returnVals = ["num"]
                            , pythonCode = "return {'num': 1}"
                            }
              )
            , ( "Space" 
              , BuiltinFunc { name = "Space"
                            , params = []
                            , returnVals = ["space"]
                            , pythonCode = "return {'space': ' '}"
                            }
              )
            , ( "Plus" 
              , BuiltinFunc { name = "Plus"
                            , params = ["a", "b"]
                            , returnVals = ["res"]
                            , pythonCode = "return {'res': a + b}"
                            }
              )
            , ( "Tup2" 
              , BuiltinFunc { name = "Tup2"
                            , params = ["first", "second"]
                            , returnVals = ["tup"]
                            , pythonCode = "return {'tup': [first, second]}"
                            }
              )
            , ( "Split_on_Whitespace" 
              , BuiltinFunc { name = "Split_on_Whitespace"
                            , params = ["str"]
                            , returnVals = ["split"]
                            , pythonCode = "import re\nreturn {'split': re.split(r'\\s', str)}"
                            }
              )
            , ( "Concatenate" 
              , BuiltinFunc { name = "Concatenate"
                            , params = ["lists"]
                            , returnVals = ["result"]
                            , pythonCode = "return {'result': [x for x in l for l in lists]}"
                            }
              )
            , ( "Reduce_by_Key" 
              , BuiltinFunc { name = "Reduce_by_Key"
                            , params = ["combiner", "initial", "items"]
                            , returnVals = ["results"]
                            , pythonCode = "results = {}\narg_names = combiner.func_code.co_varnames[0:2]\ndef combiner_wrapper(state, item):\n  result = log_call(combiner, 'reduce_by_key_internal', {arg_names[0]: state, arg_names[1]: item})\n  return result.values()[0]\nfor key, value in items:\n  if key not in results:\n    results[key] = initial\n  results[key] = combiner_wrapper(results[key], value)\nreturn {'results': results}"
                            }
              )
            , ("Moby_Dick_url"
              , BuiltinFunc { name = "Moby_Dick_url"
                            , params = []
                            , returnVals = ["url"]
                            , pythonCode = "return {'url': 'https://gist.githubusercontent.com/vilterp/26d34dd9428d79efef7d/raw/cb39d2d14c66be1ab0748de597f98d4f04abde7e/gistfile1.txt'}"
                            }
              )
            , ("HTTP_get"
              , BuiltinFunc { name = "HTTP_get"
                            , params = ["url"]
                            , returnVals = ["response_text"]
                            , pythonCode = "import requests\nreturn {'response_text': requests.get(url).text}"
                            }
              )
            ]
    , userFuncs =
        D.fromList
          [ ("main"
            , UserFunc { name = "main"
                       , graph = testGraph
                       }
            )
          , ("excl_and_hi"
            , UserFunc { name = "excl_and_hi"
                       , graph = emptyGraph
                       }
            )
          , ("word_count"
            , UserFunc { name = "word_count"
                       , graph = emptyGraph
                       }
            )
          ]
    }

-- real node ids will just be like "ap4" -- function names
-- are just for readability of this example

testGraph : Graph
testGraph =
    { nodes =
        D.fromList
            [ ("ap0_names", { pos = (-200, -200)
                            , id = "ap0_names"
                            , node = ApNode "names"
                            })
            , ("lambda0"
              , { pos = (-300, 300)
                , id = "lambda0"
                , node = LambdaNode
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
                    , dims = { width = 200, height = 150 }
                    }
                }
              )
            , ("ap3_map", { pos = (100, 0)
                          , id = "ap3_map"
                          , node = ApNode "Map"
                          })
            , ("ap4_join", { pos = (300, 0)
                                   , id = "ap4_join"
                                   , node = ApNode "join"
                                   })
            ]
    , edges =
        [ { from = (["ap0_names"], ApResultSlot "names")
          , to = (["ap3_map"], ApParamSlot "list")
          }
        , { from = (["lambda0"], FuncValueSlot)
          , to = (["ap3_map"], ApParamSlot "function")
          }
        , { from = (["ap3_map"], ApResultSlot "mappedList")
          , to = (["ap4_join"], ApParamSlot "list")
          }
        , { from = (["lambda0", "ap1_add_excl"], ApResultSlot "added")
          , to = (["lambda0", "ap2_add_hi"], ApParamSlot "str")
          }
        ]
    , nextApId = 4
    , nextLambdaId = 1
    }

code = Codegen.moduleToPython "main" helloMap
