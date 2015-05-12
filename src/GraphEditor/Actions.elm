module GraphEditor.Actions where

import Debug

import List as L

import Diagrams.Interact exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Actions exposing (..)

import Model exposing (..)
import Util exposing (..)

posNodeActions nodePath dragState =
    case dragState of
      Nothing -> { emptyActionSet | mouseDown <- Just <| stopBubbling <|
                                      \(MouseEvent evt) -> [InternalAction <| DragNodeStart { nodePath = nodePath, offset = evt.offset }] }
      _ -> emptyActionSet

nodeXOutActions : NodePath -> ActionSet Tag GraphEditorAction
nodeXOutActions nodePath = { emptyActionSet | click <- Just <| keepBubbling <| always <| [ExternalAction <| RemoveNode nodePath] }

edgeXOutActions edge = { emptyActionSet | click <- Just <| keepBubbling <| always <| [ExternalAction <| RemoveEdge edge] }

topLevelActions state =
    case state.graphEditorState.dragState of
      Just (DragPanning _) ->
          { emptyActionSet | mouseMove <- Just <|
                                keepBubbling <| \(MouseEvent evt) -> [InternalAction <| PanTo evt.offset]
                           , mouseUp <- Just <| stopBubbling <| always <| [InternalAction DragEnd] }
      _ -> emptyActionSet

canvasActions : NodePath -> Maybe DraggingState -> ActionSet Tag GraphEditorAction
canvasActions nodePath dragState =
    case dragState of
      Nothing ->
          if nodePath == []
          then { emptyActionSet | mouseDown <- Just <|
                  stopBubbling <| \(MouseEvent evt) -> [InternalAction <| PanStart { offset = evt.offset }] }
          else emptyActionSet
      Just dragging ->
          case dragging of
            DraggingNode attrs ->
               if | attrs.nodePath `directlyUnder` nodePath ->
                        { emptyActionSet | mouseMove <- 
                                              (\(MouseEvent evt) -> [ExternalAction <| MoveNode attrs.nodePath (evt.offset `pointSubtract` attrs.offset)])
                                                        |> stopBubbling |> Just
                                         , mouseUp <- Just <| stopBubbling <| always <| [InternalAction DragEnd] }
                  | attrs.nodePath `atOrAbove` nodePath ->
                        { emptyActionSet | mouseEnter <- Just <| keepBubbling <| always <| [InternalAction <| OverLambda nodePath]
                                         , mouseLeave <- Just <| keepBubbling <| always <| [InternalAction <| NotOverLambda nodePath]
                                         , mouseUp <- Just <| keepBubbling <|
                                            (\(MouseEvent evt) -> [ExternalAction <|
                                                                      DropNodeInLambda { lambdaPath = nodePath
                                                                                       , droppedNodePath = attrs.nodePath
                                                                                       , posInLambda = evt.offset }]) }
                  | otherwise -> emptyActionSet
            DraggingEdge attrs ->
               if nodePath == []
               then { emptyActionSet | mouseMove <- 
                                          (\(MouseEvent evt) -> [InternalAction <| DragEdgeTo evt.offset])
                                                |> stopBubbling |> Just
                                 , mouseUp <- Just <| stopBubbling <| always <| [InternalAction DragEnd] }
               else emptyActionSet
            _ -> emptyActionSet

atOrAbove xs ys = (xs /= ys) && (L.length xs <= L.length ys)

directlyUnder xs ys = L.length xs - 1 == L.length ys

-- TODO: check state
outPortActions : State -> OutPortId -> ActionSet Tag GraphEditorAction
outPortActions state portId =
    if outPortState state portId == NormalPort
    then { emptyActionSet | mouseDown <- Just <| stopBubbling <|
              (\evt -> case mousePosAtPath evt [TopLevel, Canvas] of
                         Just pos -> [InternalAction <| DragEdgeStart { fromPort = portId, endPos = pos } ]
                         Nothing -> Debug.crash "mouse pos not found derp") }
    else emptyActionSet

inPortActions : State -> InPortId -> ActionSet Tag GraphEditorAction
inPortActions state portId =
    let portState = inPortState state portId
    in case state.graphEditorState.dragState of
        Just (DraggingEdge attrs) ->
            if portState == ValidPort
            then { emptyActionSet | mouseUp <- Just <| stopBubbling
                      <| always <| [ ExternalAction <| AddEdge { from = attrs.fromPort, to = portId }
                                   , InternalAction DragEnd
                                   ] }
            else emptyActionSet
        _ -> emptyActionSet
