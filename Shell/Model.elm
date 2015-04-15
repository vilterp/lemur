module Shell.Model where

import Diagrams.Wiring as DW

import Debug

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
      -- BUG: this is specific to my 13" macbook pro screen, with full screen chrome.
      -- need to look at window dims on startup
    , editorLoc = editorLocFunc { width = 1280, height = 701 }
    }

-- welp, this shouldn't really be here, but..
-- BUG: 
editorLocFunc : DW.CollageLocFunc
editorLocFunc windowDims =
    let d = Debug.log "dims" windowDims
    in { offset = (252, 101), dims = { width = windowDims.width - 501, height = windowDims.height - 101 } }


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
