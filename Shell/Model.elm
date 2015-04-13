module Shell.Model where

import GraphEditor.Model as GEM

type Update = ElemPanelUpdate SidebarAction
            | NoOp

type alias State = { sidebarState : SidebarState, graphState : GEM.State }

emptyState = { sidebarState = emptySidebarState, graphState = GEM.emptyState }

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
