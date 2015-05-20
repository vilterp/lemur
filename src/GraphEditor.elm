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
import GraphEditor.Model exposing (..)

-- just the stuff from State that is relevant to this view,
-- fetched from the dictionaries
render : GraphViewModel -> DC.Diagram Tag GraphEditorAction
render viewModel = GEV.viewGraph viewModel

view : State -> Html.Html
view state =
    let viewModel = makeViewModel state
        dims = state.collageLoc.dims
    in [DC.render viewModel.editorState.diagram]
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

update : GraphEditorInternalAction -> GraphViewModel -> GraphEditorState
update action viewModel =
    let geState = viewModel.editorState
    in case action of
      -- dragging
      DragNodeStart attrs ->
          { geState | mouseInteractionState <- Just <| Dragging <| DraggingNode { attrs | overLambdaNode = Nothing } }
      DragEdgeStart attrs ->
          { geState | mouseInteractionState <- Just <| Dragging <|
              DraggingEdge { attrs | upstreamNodes = upstreamNodes viewModel.currentGraph (fst attrs.fromPort) } }
      PanStart {offset} ->
          { geState | mouseInteractionState <- Just <| Dragging <| DragPanning { offset = offset } }
      PanTo point ->
          case geState.mouseInteractionState of
            Just (Dragging (DragPanning {offset})) ->
                { geState | pan <- point `DG.pointSubtract` offset }
            _ -> Debug.crash "unexpected event"
      DragEdgeTo mousePos ->
          case geState.mouseInteractionState of
            Just (Dragging (DraggingEdge attrs)) ->
                { geState | mouseInteractionState <- Just <| Dragging <| DraggingEdge { attrs | endPos <- mousePos } }
            _ -> Debug.crash "unexpected event"
      DragEnd -> 
          { geState | mouseInteractionState <- Nothing }
      -- drag into lambdas
      OverLambda lambdaPath ->
          case geState.mouseInteractionState of
            Just (Dragging (DraggingNode attrs)) ->
                { geState | mouseInteractionState <- Just <| Dragging <| DraggingNode { attrs | overLambdaNode <- Just lambdaPath } }
            _ -> Debug.crash "unexpected event"
      NotOverLambda lambdaPath ->
          case geState.mouseInteractionState of
            Just (Dragging (DraggingNode attrs)) ->
                { geState | mouseInteractionState <- Just <| Dragging <| DraggingNode { attrs | overLambdaNode <- Nothing } }
            _ -> Debug.crash "unexpected event"
      -- port hovering
      OverInPort inPortId ->
          { geState | mouseInteractionState <- Just <| HoveringInPort inPortId }
      OverOutPort outPortId ->
          { geState | mouseInteractionState <- Just <| HoveringOutPort outPortId }
      NotOverPort ->
          { geState | mouseInteractionState <- Nothing }
