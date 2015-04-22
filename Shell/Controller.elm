module Shell.Controller where

import Debug

import GraphEditor as GE

import Shell.Model exposing (..)

update : Update -> State -> State
update up state =
  case up of
    ElemPanelUpdate action ->
        { state | sidebarState <- sidebarUpdate action state.sidebarState }
    GraphEditorUpdate evt ->
        { state | graphState <- GE.update evt state.graphState }
    NoOp -> state

sidebarUpdate : SidebarAction -> SidebarState -> SidebarState
sidebarUpdate action state =
    case (Debug.log "action" action) of
      ClickOn pos -> { state | selection <- Just <| ElemSelection pos }
      -- TODO
      Down -> state
      Up -> state
      Left -> state
      Filter str -> state
      SidebarNoOp -> state
