module GraphEditor where

import Debug

import Dict as D

import Html
import Graphics.Collage

import Diagrams.Interact as DI
import Diagrams.Geom as DG
import Diagrams.Core as DC
import Diagrams.Wiring as DW

import GraphEditor.View as GEV
import Util exposing (..)

import Model exposing (..)

-- just the stuff from State that is relevant to this view,
-- fetched from the dictionaries
type alias GraphViewModel
    { mod : Module
    , currentName : FuncName
    , currentGraph : Graph
    , collageLoc : CollageLocation
    , editorState : GraphEditorState
    , mode : GraphViewModeDenorm
    }

type GraphViewModeDenorm
    = EditingModeDenorm
    | ViewingRunModeDenorm RunId Run

makeViewModel : State -> GraphViewModel
makeViewModel state =
    case state.viewState of
      ViewingGraph attrs ->
          { mod = state.mod
          , currentName = attrs.name
          , currentGraph =
                mod.userFuncs
                  |> D.get attrs.name
                  |> getMaybeOrCrash "no such user func"
                  |> .graph
          , collageLoc = state.collageLoc
          , editorState = attrs.graphEditorState
          , mode =
              case attrs.mode of
                EditingMode -> EditingModeDenorm
                ViewingRunMode runId ->
                  ViewingRunModeDenorm runId (getRunOrCrash state runId)
          }
      EditingBuiltin _ -> Debug.crash "expecting graph viewing state"

render : State -> DC.Diagram Tag GraphEditorAction
render state = GEV.viewGraph state

view : State -> Html.Html
view state =
    let viewModel = makeViewModel state
        geState = state.graphEditorState
        dims = geState.collageLoc.dims
    in [DC.render geState.diagram]
          |> Graphics.Collage.collage (round dims.width) (round dims.height)
          |> Html.fromElement


initState : GraphEditorState
initState =
    { diagram = DC.empty -- ...
    , mouseState = DI.initMouseState
    , mouseInteractionState = Nothing
    , pan = (0, 0)
    }

-- TODO: make this in pre-panned space
defaultPos = (0, 0)

update : GraphEditorInternalAction -> State -> State
update action state =
   let geState = state.graphEditorState
       dragState = geState.dragState
       updateDragState : Maybe MouseInteractionState -> State
       updateDragState mds =
           { state | graphEditorState <- { geState | dragState <- mds } }
   in case action of
      -- dragging
      DragNodeStart attrs ->
          updateDragState <| Just <| Dragging <| DraggingNode { attrs | overLambdaNode = Nothing }
      DragEdgeStart attrs ->
          updateDragState <| Just <| Dragging <|
              DraggingEdge { attrs | upstreamNodes = upstreamNodes (state |> getCurrentGraph) (fst attrs.fromPort) }
      PanStart {offset} ->
          updateDragState <| Just <| Dragging <| DragPanning { offset = offset }
      PanTo point ->
          case state.graphEditorState.dragState of
            Just (DragPanning {offset}) ->
                { state | graphEditorState <- { geState | pan <- point `DG.pointSubtract` offset } }
            _ -> Debug.crash "unexpected event"
      DragEdgeTo mousePos ->
          case state.graphEditorState.dragState of
            Just (DraggingEdge attrs) ->
                updateDragState <| Just <| Dragging <| DraggingEdge { attrs | endPos <- mousePos }
            _ -> Debug.crash "unexpected event"
      DragEnd -> 
          updateDragState Nothing
      -- drag into lambdas
      OverLambda lambdaPath ->
          case state.graphEditorState.dragState of
            Just (DraggingNode attrs) ->
                updateDragState <| Just <| Dragging <| DraggingNode { attrs | overLambdaNode <- Just lambdaPath }
            _ -> Debug.crash "unexpected event"
      NotOverLambda lambdaPath ->
          case state.graphEditorState.dragState of
            Just (DraggingNode attrs) ->
                updateDragState <| Just <| Dragging <| DraggingNode { attrs | overLambdaNode <- Nothing }
            _ -> Debug.crash "unexpected event"
      -- port hovering
      OverInPort inPortId ->
          updateDragState <| Just <| HoveringInPort inPortId
      OverOutPort outPortId ->
          updateDragState <| Just <| HoveringOutPort outPortId
      NotOverPort ->
          updateDragState <| Nothing
