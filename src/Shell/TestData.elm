module Shell.TestData where

import Shell.Model exposing (..)
import Shell.Module exposing (..)
import GraphEditor.Model as GEM

import Dict as D

dumbTestMod : Module
dumbTestMod =
    { name = "DumbTest"
    , builtinFuncs =
        D.fromList
            [ ("someList", { name = "someList"
                           , params = []
                           , pythonCode = "return ['foo', 'bar', 'baz']"
                           })
            , ("map", { name = "map"
                      , params = ["function", "list"]
                      , pythonCode = "return map(function, list)"
                      })
            , ("addExcl", { name = "addExcl"
                          , params = ["str"]
                          , pythonCode = "return str + '!'"
                          })
            , ("commaJoin", { name = "commaJoin"
                            , params = ["list"]
                            , pythonCode = "return ', '.join(list)"
                            })
            ]
    , userFuncs = D.fromList [("main", GEM.emptyGraph)]
    }

sidebarModules = [dumbTestMod]

initSidebarState = { emptySidebarState | modules <- sidebarModules }

initState = { emptyState | sidebarState <- initSidebarState }
