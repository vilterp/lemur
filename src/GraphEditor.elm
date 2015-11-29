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
import Model.Graph exposing (..)
import GraphEditor.Model exposing (..)

-- just the stuff from State that is relevant to this view,
-- fetched from the dictionaries
render : GraphViewModel -> GraphViewModel
render viewModel =
    let editorState = viewModel.editorState
    in { viewModel | editorState =
                        { editorState | diagram = GEV.viewGraph viewModel } }

view : DW.CollageLocation -> GraphEditorState -> Html.Html
view collageLoc editorState =
    let dims = collageLoc.dims
    in [DC.render editorState.diagram]
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

-- NOTE: doesn't re-render
update : GraphEditorInternalAction -> GraphViewModel -> GraphViewModel
update action viewModel =
    let geState = viewModel.editorState
        newGeState =
            case action of
              -- dragging
              DragNodeStart attrs ->
                  { geState | mouseInteractionState = Just <| Dragging <|
                      DraggingNode
                        { nodePath = attrs.nodePath
                        , offset = attrs.offset
                        , overLambdaNode = Nothing
                        }
                  }
              DragEdgeStart attrs ->
                  { geState | mouseInteractionState = Just <| Dragging <|
                      DraggingEdge
                        { fromPort = attrs.fromPort
                        , endPos = attrs.endPos
                        , upstreamNodes = upstreamNodes viewModel.currentGraph (fst attrs.fromPort)
                        }
                  }
              PanStart {offset} ->
                  { geState | mouseInteractionState = Just <| Dragging <| DragPanning { offset = offset } }
              PanTo point ->
                  case geState.mouseInteractionState of
                    Just (Dragging (DragPanning {offset})) ->
                        { geState | pan = point `DG.pointSubtract` offset }
                    _ -> Debug.crash "unexpected event"
              DragEdgeTo mousePos ->
                  case geState.mouseInteractionState of
                    Just (Dragging (DraggingEdge attrs)) ->
                        { geState | mouseInteractionState = Just <| Dragging <| DraggingEdge { attrs | endPos = mousePos } }
                    _ -> Debug.crash "unexpected event"
              DragEnd -> 
                  { geState | mouseInteractionState = Nothing }
              -- drag into lambdas
              OverLambda lambdaPath ->
                  case geState.mouseInteractionState of
                    Just (Dragging (DraggingNode attrs)) ->
                        { geState | mouseInteractionState = Just <| Dragging <| DraggingNode { attrs | overLambdaNode = Just lambdaPath } }
                    _ -> Debug.crash "unexpected event"
              NotOverLambda lambdaPath ->
                  case geState.mouseInteractionState of
                    Just (Dragging (DraggingNode attrs)) ->
                        { geState | mouseInteractionState = Just <| Dragging <| DraggingNode { attrs | overLambdaNode = Nothing } }
                    _ -> Debug.crash "unexpected event"
              -- port hovering
              OverInPort inPortId ->
                  { geState | mouseInteractionState = Just <| HoveringInPort inPortId }
              OverOutPort outPortId ->
                  { geState | mouseInteractionState = Just <| HoveringOutPort outPortId }
              NotOverPort ->
                  { geState | mouseInteractionState = Nothing }
    in { viewModel | editorState = newGeState }

-- NOTE: doesn't re-render
updateGraph : GraphAction -> GraphViewModel -> GraphViewModel
updateGraph act viewModel =
    case act of
      MoveNode nodePath point ->
          viewModel |> updateGraphFun (moveNode nodePath point)
      AddLambda ->
          let (newGraph, lambdaIdNo) = viewModel.currentGraph |> getApId
              newVM = viewModel |> updateGraphFun (always <| Ok newGraph)
              lambdaId = "lambda" ++ toString lambdaIdNo
              posNode = { pos = defaultPos
                        , id = lambdaId
                        , node = emptyLambdaNode
                        }
          in newVM |> updateGraphFun (addNode [lambdaId] posNode)
      AddApNode funcId ->
          let (newGraph, apIdNo) = viewModel.currentGraph |> getLambdaId
              newVM = viewModel |> updateGraphFun (always <| Ok newGraph)
              apId = "ap" ++ toString apIdNo
              posNode = { pos = defaultPos
                        , id = apId
                        , node = ApNode funcId
                        }
          in newVM |> updateGraphFun (addNode [apId] posNode)
      RemoveNode nodePath ->
          viewModel |> updateGraphFun (removeNode nodePath)
      AddEdge edge ->
          viewModel |> updateGraphFun (addEdge edge)
      RemoveEdge edge ->
          viewModel |> updateGraphFun (removeEdge edge)
      DropNodeInLambda {lambdaPath, droppedNodePath, posInLambda} ->
          if canBeDroppedInLambda viewModel.currentGraph lambdaPath droppedNodePath
          then viewModel |> updateGraphFun (moveNodeToLambda lambdaPath droppedNodePath posInLambda)
          else viewModel
