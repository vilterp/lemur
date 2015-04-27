module GraphEditor where

import Debug

import Html
import Graphics.Collage as C

import Diagrams.Interact as DI
import Diagrams.Geom as DG
import Diagrams.Core as D
import Diagrams.Wiring as DW

import GraphEditor.Model as GEM
import GraphEditor.View as GEV
import GraphEditor.Controller as GEC
import Util exposing (..)

import Model

type alias State =
    { intState : DI.InteractionState GEM.State GEM.Tag GEM.Action
    , editorLoc : DW.CollageLocation
    }

type GraphEditorEvt
    = MouseUpdate (DW.CollageLocation, DW.PrimMouseEvent)
    | ModuleUpdate Model.Module

initState : GEM.State -> State
initState state =
    { intState = DI.initInteractState GEC.update GEV.viewGraph state
    -- BUG: this is specific to my 13" macbook pro screen, with full screen chrome.
    -- need to look at window dims on startup
    , editorLoc = editorLocFunc { width = 1280, height = 701 }
    }

-- TODO: make this in pre-panned space
defaultPos = (0, 0)

editorLocFunc : DW.CollageLocFunc
editorLocFunc windowDims =
    let d = Debug.log "dims" windowDims
    in { offset = (252, 101)
       , dims = { width = windowDims.width - 501
                , height = windowDims.height - 101
                }
       }

update : GraphEditorEvt -> State -> State
update evt state =
    case evt of
      MouseUpdate mouseEvt ->
          { state | intState <- DI.update mouseEvt state.intState }
      ModuleUpdate newMod ->
          { state | intState <- DI.updateModel (\geState -> { geState | mod <- newMod }) state.intState }

view : State -> Html.Html
view state =
    let dims = state.editorLoc.dims
    in [D.render state.intState.diagram]
          |> C.collage (round dims.width) (round dims.height)
          |> Html.fromElement
