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
    , pythonFuncs =
        D.fromList
            [ ( "names"
              , PythonFunc { name = "names"
                            , params = []
                            , returnVals = ["names"]
                            , pythonCode = "return {'names':['Pete', 'Borja', 'Ravi', 'Mike']}"
                            }
              )
            , ( "Map"
              , PythonFunc { name = "Map"
                            , params = ["function", "list"]
                            , returnVals = ["mappedList"]
                            , pythonCode = "arg_name = function.func_code.co_varnames[0]\ndef apply_single(item):\n  result = log_call(function, 'map_internal', {arg_name: item})\n  return result.values()[0]\nreturn {'mappedList': map(apply_single, list)}"
                            }
              )
            , ( "add_excl"
              , PythonFunc { name = "add_excl"
                            , params = ["str"]
                            , returnVals = ["added"]
                            , pythonCode = "return {'added':str + '!'}"
                            }
              )
            , ( "length"
              , PythonFunc { name = "length"
                            , params = ["str"]
                            , returnVals = ["length"]
                            , pythonCode = "return {'length':len(str)}"
                            }
              )
            , ( "add_hi"
              , PythonFunc { name = "add_hi"
                            , params = ["str"]
                            , returnVals = ["added"]
                            , pythonCode = "return {'added': 'Hi ' + str}"
                            }
              )
            -- TODO: define this in terms of join;
            -- define join in terms of fold
            , ( "join"
              , PythonFunc { name = "join"
                            , params = ["joiner", "list"]
                            , returnVals = ["joinedList"]
                            , pythonCode = "return {'joinedList': joiner.join(list)}"
                            }
              )
            , ( "Zero" 
              , PythonFunc { name = "Zero"
                            , params = []
                            , returnVals = ["num"]
                            , pythonCode = "return {'num': 0}"
                            }
              )
            , ( "One" 
              , PythonFunc { name = "One"
                            , params = []
                            , returnVals = ["num"]
                            , pythonCode = "return {'num': 1}"
                            }
              )
            , ( "Space" 
              , PythonFunc { name = "Space"
                            , params = []
                            , returnVals = ["space"]
                            , pythonCode = "return {'space': ' '}"
                            }
              )
            , ( "Plus" 
              , PythonFunc { name = "Plus"
                            , params = ["a", "b"]
                            , returnVals = ["res"]
                            , pythonCode = "return {'res': a + b}"
                            }
              )
            , ( "Tup2" 
              , PythonFunc { name = "Tup2"
                            , params = ["first", "second"]
                            , returnVals = ["tup"]
                            , pythonCode = "return {'tup': [first, second]}"
                            }
              )
            , ( "Split_on_Whitespace" 
              , PythonFunc { name = "Split_on_Whitespace"
                            , params = ["str"]
                            , returnVals = ["split"]
                            , pythonCode = "import re\nreturn {'split': re.split(r'\\s', str)}"
                            }
              )
            , ( "Concatenate" 
              , PythonFunc { name = "Concatenate"
                            , params = ["lists"]
                            , returnVals = ["result"]
                            , pythonCode = "return {'result': [x for x in l for l in lists]}"
                            }
              )
            , ( "Reduce_by_Key" 
              , PythonFunc { name = "Reduce_by_Key"
                            , params = ["combiner", "initial", "items"]
                            , returnVals = ["results"]
                            , pythonCode = "results = {}\narg_names = combiner.func_code.co_varnames[0:2]\ndef combiner_wrapper(state, item):\n  result = log_call(combiner, 'reduce_by_key_internal', {arg_names[0]: state, arg_names[1]: item})\n  return result.values()[0]\nfor key, value in items:\n  if key not in results:\n    results[key] = initial\n  results[key] = combiner_wrapper(results[key], value)\nreturn {'results': results}"
                            }
              )
            , ("gettysburg_url"
              , PythonFunc { name = "gettysburg_url"
                            , params = []
                            , returnVals = ["url"]
                            , pythonCode = "return {'url': 'https://gist.githubusercontent.com/vilterp/d81dba5421069e9c4ac4/raw/751a80c01676614b0323e36777164131aaaf4596/gettysburg.txt'}"
                            }
              )
            , ("HTTP_get"
              , PythonFunc { name = "HTTP_get"
                            , params = ["url"]
                            , returnVals = ["response_text"]
                            , pythonCode = "import requests\nreturn {'response_text': requests.get(url).text}"
                            }
              )
            ]
    , graphFuncs =
        D.fromList
          [ ("main"
            , GraphFunc { name = "main"
                       , graph = testGraph
                       }
            )
          , ("excl_and_hi"
            , GraphFunc { name = "excl_and_hi"
                       , graph = emptyGraph
                       }
            )
          , ("word_count"
            , GraphFunc { name = "word_count"
                       , graph = emptyGraph
                       }
            )
          , ("word_count_test"
            , GraphFunc { name = "word_count_test"
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
