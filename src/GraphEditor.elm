module GraphEditor where

import Debug

import Html
import Graphics.Collage

import Diagrams.Interact as DI
import Diagrams.Geom as DG
import Diagrams.Core as DC
import Diagrams.Wiring as DW

import GraphEditor.View as GEV
import Util exposing (..)

import Model exposing (..)

view : Model.State -> Html.Html
view state =
    let geState = state.graphEditorState
        dims = geState.collageDims
    in [DC.render geState.diagram]
          |> Graphics.Collage.collage (round dims.width) (round dims.height)
          |> Html.fromElement


initState : GraphEditorState
initState =
    { diagram = DC.empty -- ...
    , mouseState = DI.initMouseState
    , collageDims = { width = 500, height = 500 } -- ...
    , dragState = Nothing
    , pan = (0, 0)
    }

-- TODO: make this in pre-panned space
defaultPos = (0, 0)

-- TODO: need to recalibrate this
editorLocFunc : DW.CollageLocFunc
editorLocFunc windowDims =
    let d = Debug.log "dims" windowDims
    in { offset = (252, 101)
       , dims = { width = windowDims.width - 501
                , height = windowDims.height - 101
                }
       }

update : GraphEditorInternalAction -> State -> State
update action state =
   let geState = state.graphEditorState
       dragState = geState.dragState
       updateDragState : Maybe DraggingState -> State
       updateDragState mds =
           { state | graphEditorState <- { geState | dragState <- mds } }
   in case action of
     -- dragging
     DragNodeStart attrs ->
         updateDragState <| Just <| DraggingNode { attrs | overLambdaNode = Nothing }
     DragEdgeStart attrs ->
         updateDragState <| Just <|
             DraggingEdge { attrs | upstreamNodes = upstreamNodes (state |> getCurrentGraph) (fst attrs.fromPort) }
     PanStart {offset} ->
         updateDragState <| Just <| DragPanning { offset = offset }
     PanTo point ->
         case state.graphEditorState.dragState of
           Just (DragPanning {offset}) ->
               { state | graphEditorState <- { geState | pan <- point `DG.pointSubtract` offset } }
           _ -> Debug.crash "unexpected event"
     DragEdgeTo mousePos ->
         case state.graphEditorState.dragState of
           Just (DraggingEdge attrs) ->
               updateDragState <| Just <| DraggingEdge { attrs | endPos <- mousePos }
           _ -> Debug.crash "unexpected event"
     DragEnd -> 
         updateDragState Nothing
     -- drag into lambdas
     OverLambda lambdaPath ->
         case state.graphEditorState.dragState of
           Just (DraggingNode attrs) ->
               updateDragState <| Just <| DraggingNode { attrs | overLambdaNode <- Just lambdaPath }
           _ -> Debug.crash "unexpected event"
     NotOverLambda lambdaPath ->
         case state.graphEditorState.dragState of
           Just (DraggingNode attrs) ->
               updateDragState <| Just <| DraggingNode { attrs | overLambdaNode <- Nothing }
           _ -> Debug.crash "unexpected event"
