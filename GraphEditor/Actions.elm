module GraphEditor.Actions where

import List as L
import Debug

import Diagrams.Actions (..)

import GraphEditor.Model (..)

posNodeActions nodePath dragState =
    case dragState of
      Nothing -> { emptyActionSet | mouseDown <- Just <| stopBubbling <|
                                      \(MouseEvent evt) -> DragNodeStart { nodePath = nodePath, offset = evt.offset } }
      _ -> emptyActionSet

nodeXOutActions nodePath = { emptyActionSet | click <- Just <| keepBubbling <| always <| RemoveNode nodePath }

edgeXOutActions edge = { emptyActionSet | click <- Just <| stopBubbling <| always <| RemoveEdge edge }

canvasActions nodePath dragState =
    case dragState of
      Nothing -> emptyActionSet
      Just dragging ->
          let moveAndUp = { emptyActionSet | mouseMove <- Just
                                              <| stopBubbling <| \(MouseEvent evt) -> DragMove evt.offset
                                           , mouseUp <- Just <| stopBubbling <| always DragEnd }
          in case dragging of
               DraggingNode attrs ->
                  if attrs.nodePath `directlyUnder` nodePath then moveAndUp else emptyActionSet
               DraggingEdge attrs ->
                  if nodePath == [] then moveAndUp else emptyActionSet

directlyUnder xs ys = L.length xs - 1 == L.length ys

-- TODO: check state
outPortActions : State -> OutPortId -> ActionSet Tag Action
outPortActions state portId =
    if outPortState state portId == NormalPort
    then { emptyActionSet | mouseDown <- Just <| stopBubbling <|
              \evt -> DragEdgeStart { fromPort = portId, endPos = collageMousePos evt } }
    else emptyActionSet

inPortActions : State -> InPortId -> ActionSet Tag Action
inPortActions state portId =
    let portState = inPortState state portId
    in case state.dragState of
         Just (DraggingEdge attrs) -> if portState == ValidPort
                                      then { emptyActionSet | mouseUp <- Just <| stopBubbling
                                                <| always <| AddEdge { from = attrs.fromPort, to = portId } }
                                      else emptyActionSet
         _ -> emptyActionSet
