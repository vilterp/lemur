module GraphEditor.Controller where

import Debug

import Diagrams.Interact (..)
import Diagrams.Geom (..)

import GraphEditor.Model (..)
import GraphEditor.View (..)

-- BUG: if func out port being used, shouldn't be able to use any other ports

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

render : RenderFunc State Tag Action
render state = viewGraph state
