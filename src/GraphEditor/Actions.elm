module GraphEditor.Actions where

import Debug

import List as L

import Diagrams.Interact exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Actions exposing (..)

import Model exposing (..)
import GraphEditor.Model exposing (..)
import Util exposing (..)

onlyIfEditing : GraphViewModel -> ActionSet Tag GraphEditorAction -> ActionSet Tag GraphEditorAction
onlyIfEditing viewModel actSet =
    if isReadOnly viewModel then emptyActionSet else actSet

posNodeActions nodePath viewModel =
    (case viewModel.editorState.mouseInteractionState of
        Nothing -> { emptyActionSet | mouseDown = Just <| stopBubbling <|
                                        \(MouseEvent evt) ->
                                            [InternalAction <|
                                                DragNodeStart { nodePath = nodePath
                                                              , offset = evt.offset
                                                              }] }
        _ -> emptyActionSet)
      |> onlyIfEditing viewModel

nodeXOutActions : NodePath -> ActionSet Tag GraphEditorAction
nodeXOutActions nodePath =
  { emptyActionSet | click = Just <| keepBubbling <| always <|
                        [ExternalAction <| RemoveNode nodePath] }

edgeXOutActions edge = { emptyActionSet | click = Just <| keepBubbling <| always <| [ExternalAction <| RemoveEdge edge] }

topLevelActions mouseInteractionState =
    case mouseInteractionState of
      Just (Dragging (DragPanning _)) ->
          { emptyActionSet | mouseMove = Just <|
                                keepBubbling <| \(MouseEvent evt) -> [InternalAction <| PanTo evt.offset]
                           , mouseUp = Just <| stopBubbling <| always <| [InternalAction DragEnd] }
      _ -> emptyActionSet

canvasActions : NodePath -> Maybe MouseInteractionState -> ActionSet Tag GraphEditorAction
canvasActions nodePath mouseInteractionState =
    case mouseInteractionState of
      Nothing ->
          if nodePath == []
          then { emptyActionSet | mouseDown = Just <|
                  stopBubbling <| \(MouseEvent evt) -> [InternalAction <| PanStart { offset = evt.offset }] }
          else emptyActionSet
      Just (Dragging dragging) ->
          case dragging of
            DraggingNode attrs ->
              if attrs.nodePath `directlyUnder` nodePath then
                { emptyActionSet | mouseMove = 
                                      (\(MouseEvent evt) -> [ExternalAction <| MoveNode attrs.nodePath (evt.offset `pointSubtract` attrs.offset)])
                                                |> stopBubbling |> Just
                                 , mouseUp = Just <| stopBubbling <| always <| [InternalAction DragEnd] }
              else if attrs.nodePath `atOrAbove` nodePath then
                  { emptyActionSet | mouseEnter = Just <| keepBubbling <| always <| [InternalAction <| OverLambda nodePath]
                                   , mouseLeave = Just <| keepBubbling <| always <| [InternalAction <| NotOverLambda nodePath]
                                   , mouseUp = Just <| keepBubbling <|
                                      (\(MouseEvent evt) -> [ExternalAction <|
                                                                DropNodeInLambda { lambdaPath = nodePath
                                                                                 , droppedNodePath = attrs.nodePath
                                                                                 , posInLambda = evt.offset `pointSubtract` attrs.offset 
                                                                                 }]) }
              else
                emptyActionSet
            DraggingEdge attrs ->
               if nodePath == []
               then { emptyActionSet | mouseMove = 
                                          (\(MouseEvent evt) -> [InternalAction <| DragEdgeTo evt.offset])
                                                |> stopBubbling |> Just
                                 , mouseUp = Just <| stopBubbling <| always <| [InternalAction DragEnd] }
               else emptyActionSet
            _ -> emptyActionSet
      Just _ -> emptyActionSet

atOrAbove xs ys = (xs /= ys) && (L.length xs <= L.length ys)

directlyUnder xs ys = L.length xs - 1 == L.length ys

outPortActions : GraphViewModel -> OutPortId -> ActionSet Tag GraphEditorAction
outPortActions viewModel portId =
    let hoverActions =
          { emptyActionSet | mouseEnter = Just <| stopBubbling <| always [InternalAction <| OverOutPort portId]
                           , mouseLeave = Just <| stopBubbling <| always [InternalAction <| NotOverPort] }
    in case viewModel.mode of
      EditingModeDenorm ->
          if outPortState viewModel portId == NormalPort
          then case viewModel.editorState.mouseInteractionState of
              Just (Dragging (DraggingEdge _)) ->
                  emptyActionSet
              _ -> 
                  { hoverActions | mouseDown = Just <| stopBubbling <|
                            (\evt -> case mousePosAtPath evt [TopLevel, Canvas] of
                                        Just pos -> [ InternalAction <| NotOverPort
                                                    , InternalAction <| DragEdgeStart { fromPort = portId, endPos = pos }
                                                    ]
                                        Nothing -> Debug.crash "mouse pos not found derp") }
          else emptyActionSet
      ViewingRunModeDenorm _ _ ->
          hoverActions

inPortActions : GraphViewModel -> InPortId -> ActionSet Tag GraphEditorAction
inPortActions viewModel portId =
    let portState = inPortState viewModel portId
    in case viewModel.editorState.mouseInteractionState of
        Just (Dragging (DraggingEdge attrs)) ->
            if portState == ValidPort
            then { emptyActionSet | mouseUp = Just <| stopBubbling
                      <| always <| [ ExternalAction <| AddEdge { from = attrs.fromPort, to = portId }
                                   , InternalAction DragEnd
                                   ] }
            else emptyActionSet
        _ ->
            { emptyActionSet | mouseEnter = Just <| stopBubbling <| always [InternalAction <| OverInPort portId]
                             , mouseLeave = Just <| stopBubbling <| always [InternalAction <| NotOverPort] }
