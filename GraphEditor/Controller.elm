module GraphEditor.Controller where

import Debug

import List as L
import Dict as D

import Diagrams.Interact (..)
import Diagrams.Geom (..)
import Diagrams.Actions (..)

import GraphEditor.Model (..)
import GraphEditor.Util (..)
import GraphEditor.View (..)

updateNodeViews : NodeDict -> State -> NodeDict
updateNodeViews nodeDict state =
    D.map (\id posNode -> { posNode | cachedDiagram <- viewNode posNode.node [id] state }) nodeDict

updateGraphViews : Graph -> State -> Graph
updateGraphViews graph state =
    { graph | nodes <- updateNodeViews graph.nodes state }

updateStateViews : State -> State
updateStateViews state =
    { state | graph <- updateGraphViews state.graph state }

update : UpdateFunc State Action
update action state =
    case action of
      DragNodeStart attrs -> { state | dragState <- Just <| DraggingNode attrs }
      DragEdgeStart attrs -> updateStateViews
                                { state | dragState <- Just <| DraggingEdge
                                    { attrs | upstreamNodes = upstreamNodes state.graph (fst attrs.fromPort) } }
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
      DragEnd -> updateStateViews { state | dragState <- Nothing }
      RemoveNode nodePath ->
          case removeNode state.graph nodePath of
            Ok newGraph -> updateStateViews { state | graph <- newGraph }
            Err msg -> Debug.crash msg
      RemoveEdge edge -> updateStateViews { state | graph <- removeEdge edge state.graph }
      AddEdge edge ->
        case addEdge edge state.graph of
          Ok newGraph -> updateStateViews { state | graph <- newGraph
                                                  , dragState <- Nothing }
          Err msg -> Debug.crash msg
      NoOp -> state
