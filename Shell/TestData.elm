module Shell.TestData where

import Shell.Model exposing (..)

stdlib : SidebarModule
stdlib =
    { name = "StdLib"
    , elements =
        [ { typ = Workflow, name = "Demo"}
        , { typ = App, name = "Fold"}
        , { typ = App, name = "Filter"}
        , { typ = App, name = "Map"}
        , { typ = App, name = "Join"}
        , { typ = App, name = "Split"}
        , { typ = Datatype, name = "List"}
        , { typ = Datatype, name = "Map"}
        , { typ = Datatype, name = "Queue"}
        , { typ = Datatype, name = "Stack"}
        ]
    }

easysim : SidebarModule
easysim =
    { name = "EasySIM"
    , elements =
        [ { typ = Workflow, name = "Demo"}
        , { typ = App, name = "Convert Weather"}
        , { typ = App, name = "Convert Soil"}
        , { typ = Datatype, name = "PSims Weather"}
        , { typ = Datatype, name = "DSSAT Weather"}
        , { typ = Datatype, name = "PSims Soil"}
        , { typ = Datatype, name = "DSSAT Soil"}
        ]
    }

sidebarModules = [easysim, stdlib]

initSidebarState = { emptySidebarState | modules <- sidebarModules }

initState = { emptyState | sidebarState <- initSidebarState }
