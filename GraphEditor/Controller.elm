module GraphEditor.Controller where

import Debug

import List as L

import Diagrams.Interact (..)
import Diagrams.Geom (..)
import Diagrams.Actions (..)

import GraphEditor.Model (..)
import GraphEditor.Util (..)

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
                  let a = Debug.watch "anp" attrs.nodePath
                      b = Debug.watch "np" nodePath
                  in if attrs.nodePath `directlyUnder` nodePath then moveAndUp else emptyActionSet
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

-- process 'em...

update : UpdateFunc State Action
update action state =
    case action of
      DragNodeStart attrs -> { state | dragState <- Just <| DraggingNode attrs }
      DragEdgeStart attrs -> { state | dragState <- Just <| DraggingEdge attrs }
      DragMove mousePos ->
          case state.dragState of
            Just (DraggingNode attrs) ->
                let moveRes = moveNode state.graph attrs.nodePath
                                       (mousePos `pointSubtract` attrs.offset)
                in case moveRes of
                     Ok newGraph -> { state | graph <- newGraph }
                     Err msg -> Debug.crash msg
            Just (DraggingEdge attrs) ->
                { state | dragState <-
                            Just <| DraggingEdge { attrs | endPos <- mousePos } }
            Nothing -> state
            _ -> state
      DragEnd -> { state | dragState <- Nothing }
      RemoveNode nodePath ->
          case removeNode state.graph nodePath of
            Ok newGraph -> { state | graph <- newGraph }
            Err msg -> Debug.crash msg
      RemoveEdge edge -> { state | graph <- removeEdge edge state.graph }
      AddEdge edge ->
        case addEdge edge state.graph of
          Ok newGraph -> { state | graph <- newGraph
                                 , dragState <- Nothing }
          Err msg -> Debug.crash msg
      NoOp -> state
