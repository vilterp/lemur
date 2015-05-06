module GraphEditor.Controller where

import Debug

import List as L

import Diagrams.Interact exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Actions exposing (..)

import Model exposing (..)
import GraphEditor.Model exposing (..)
import Util exposing (..)

posNodeActions nodePath dragState =
    case dragState of
      Nothing -> { emptyActionSet | mouseDown <- Just <| stopBubbling <|
                                      \(MouseEvent evt) -> InternalAction <| DragNodeStart { nodePath = nodePath, offset = evt.offset } }
      _ -> emptyActionSet

nodeXOutActions nodePath = { emptyActionSet | click <- Just <| keepBubbling <| always <| ExternalAction <| RemoveNode nodePath }

edgeXOutActions edge = { emptyActionSet | click <- Just <| keepBubbling <| always <| ExternalAction <| RemoveEdge edge }

topLevelActions state =
    case state.dragState of
      Just (DragPanning _) ->
          { emptyActionSet | mouseMove <- Just <|
                                keepBubbling <| \(MouseEvent evt) -> InternalAction <| PanTo evt.offset
                           , mouseUp <- Just <| stopBubbling <| always <| InternalAction DragEnd }
      _ -> emptyActionSet

canvasActions nodePath dragState =
    case dragState of
      Nothing ->
          if nodePath == []
          then { emptyActionSet | mouseDown <- Just <|
                  stopBubbling <| \(MouseEvent evt) -> InternalAction <| PanStart { offset = evt.offset } }
          else emptyActionSet
      Just dragging ->
          let moveAndUp isNode =
                { emptyActionSet | mouseMove <- 
                                      (\(MouseEvent evt) -> if isNode
                                                            then ExternalAction <| MoveNode nodePath evt.offset
                                                            else InternalAction <| DragEdgeTo evt.offset)
                                        |> stopBubbling |> Just
                                 , mouseUp <- Just <| stopBubbling <| always DragEnd }
          in case dragging of
               DraggingNode attrs ->
                  if | attrs.nodePath `directlyUnder` nodePath -> moveAndUp True
                     | attrs.nodePath `atOrAbove` nodePath ->
                          { emptyActionSet | mouseEnter <- Just <| keepBubbling <| always <| InternalAction <| OverLambda nodePath
                                           , mouseLeave <- Just <| keepBubbling <| always <| InternalAction <| NotOverLambda nodePath
                                           , mouseUp <- Just <| keepBubbling <|
                                              (\(MouseEvent evt) -> ExternalAction <|
                                                    DropNodeInLambda { lambdaPath = nodePath
                                                                     , droppedNodePath = attrs.nodePath
                                                                     , posInLambda = evt.offset }) }
                     | otherwise -> emptyActionSet
               DraggingEdge attrs ->
                  if nodePath == []
                  then moveAndUp False
                  else emptyActionSet
               _ -> emptyActionSet

atOrAbove xs ys = (xs /= ys) && (L.length xs <= L.length ys)

directlyUnder xs ys = L.length xs - 1 == L.length ys

-- TODO: check state
outPortActions : State -> OutPortId -> ActionSet Tag Action
outPortActions state portId =
    if outPortState state portId == NormalPort
    then { emptyActionSet | mouseDown <- Just <| stopBubbling <|
              (\evt -> case mousePosAtPath evt [TopLevel, Canvas] of
                         Just pos -> InternalAction <| DragEdgeStart { fromPort = portId, endPos = pos }
                         Nothing -> Debug.crash "mouse pos not found derp") }
    else emptyActionSet

inPortActions : State -> InPortId -> ActionSet Tag Action
inPortActions state portId =
    let portState = inPortState state portId
    in case state.dragState of
         Just (DraggingEdge attrs) -> if portState == ValidPort
                                      then { emptyActionSet | mouseUp <- Just <| stopBubbling
                                                <| always <| ExternalAction <| AddEdge { from = attrs.fromPort, to = portId } }
                                      else emptyActionSet
         _ -> emptyActionSet

-- process 'em...

update : GraphEditorInternalAction -> State -> State
update action state =
    case action of
      -- dragging
      DragNodeStart attrs -> { state | dragState <- Just <| DraggingNode { attrs | overLambdaNode = Nothing } }
      DragEdgeStart attrs -> { state | dragState <- Just <|
          DraggingEdge { attrs | upstreamNodes = upstreamNodes (state |> getGraph) (fst attrs.fromPort) } }
      PanStart {offset} -> { state | dragState <- Just <| DragPanning { offset = offset } }
      PanTo point ->
          case state.dragState of
            Just (DragPanning {offset}) ->
                { state | pan <- mousePos `pointSubtract` offset }
            _ -> Debug.crash "unexpected event"
      DragEdgeTo point ->
          case state.dragState of
            Just (DraggingEdge attrs) ->
                { state | dragState <-
                            Just <| DraggingEdge { attrs | endPos <- mousePos } }
            _ -> Debug.crash "unexpected event"
      DragEnd -> { state | dragState <- Nothing }
      -- drag into lambdas
      OverLambda lambdaPath ->
          case state.dragState of
            Just (DraggingNode attrs) ->
                let ds = state.dragState
                in { state | dragState <- Just <| DraggingNode { attrs | overLambdaNode <- Just lambdaPath } }
            _ -> Debug.crash "unexpected event"
      NotOverLambda lambdaPath ->
          case state.dragState of
            Just (DraggingNode attrs) ->
                let ds = state.dragState
                in { state | dragState <- Just <| DraggingNode { attrs | overLambdaNode <- Nothing } }
            _ -> Debug.crash "unexpected event"
