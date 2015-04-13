module Shell.Model where

import Diagrams.Wiring as DW

import GraphEditor as GE
import GraphEditor.Model as GEM
import GraphEditor.TestData as GETD

type Update = ElemPanelUpdate SidebarAction
            | MouseUpdate (DW.CollageLocation, DW.PrimMouseEvent)
            | NoOp

type alias State = { sidebarState : SidebarState, graphState : GE.State, editorLoc : DW.CollageLocation }

emptyState =
    { sidebarState = emptySidebarState
    , graphState = GE.initState GETD.initState
    , editorLoc = { offset = (252, 101), dims = { width = 800, height = 800 } }
    }

type alias SidebarState = { modules : List SidebarModule, selection : Maybe SidebarSelection }

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
