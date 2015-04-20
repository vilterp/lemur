module Shell.Model where

import Diagrams.Wiring as DW

import Debug

import GraphEditor as GE
import GraphEditor.TestData as GETD

type Update
    = ElemPanelUpdate SidebarAction
    | GraphEditorUpdate GE.GraphEditorEvt
    | NoOp

-- TODO: move CollageLocation to GraphEditor state
type alias State =
    { sidebarState : SidebarState
    , graphState : GE.State
    }

emptyState =
    { sidebarState = emptySidebarState
    , graphState = GE.initState GETD.initState
    }

type alias SidebarState =
    { modules : List SidebarModule
    , selection : Maybe SidebarSelection
    }

emptySidebarState = { modules = [], selection = Nothing }

type SidebarSelection = ElemSelection SidebarElemPos

type alias SidebarElemPos = { modIdx : Int, elemIdx : Int }

type SidebarAction = ClickOn SidebarElemPos
                   | Down
                   | Up
                   | Left
                   | Filter String
                   | SidebarNoOp

type alias SidebarModule = { name : String, elements : List ModuleElement }
type alias ModuleElement = { typ : ElementType, name : String }
type ElementType = Workflow | App | Datatype
