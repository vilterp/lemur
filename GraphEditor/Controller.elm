module GraphEditor.Controller where

import Debug

import List as L
import Dict as D
import Result as R

import Diagrams.Interact (..)
import Diagrams.Geom (..)
import Diagrams.Actions (..)

import GraphEditor.Model (..)
import GraphEditor.Util (..)
import GraphEditor.View (..)

edgeXOutActions edge = { emptyActionSet | click <- Just <| keepBubbling <| always <| RemoveEdge edge }

canvasActions nodePath dragState =
    case dragState of
      Nothing -> emptyActionSet
      Just dragging ->
          let moveAndUp = { emptyActionSet | mouseMove <- Just
                                              <| stopBubbling <| \(MouseEvent evt) -> DragMove evt.offset
                                           , mouseUp <- Just <| stopBubbling <| always DragEnd }
          in case dragging of
               DraggingNode attrs ->
                  if | attrs.nodePath `directlyUnder` nodePath -> moveAndUp 
                     | attrs.nodePath `atOrAbove` nodePath ->
                          { emptyActionSet | mouseEnter <- Just <| keepBubbling <| always <| OverLambda nodePath
                                           , mouseLeave <- Just <| keepBubbling <| always <| NotOverLambda nodePath
                                           , mouseUp <- Just <| keepBubbling <| always <|
                                                DropNodeInLambda { lambdaPath = nodePath, droppedNodePath = attrs.nodePath } }
                     | otherwise -> emptyActionSet
               DraggingEdge attrs ->
                  if nodePath == [] then moveAndUp else emptyActionSet

atOrAbove xs ys = (xs /= ys) && (L.length xs <= L.length ys)

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
      -- dragging
      DragNodeStart attrs -> updateStateViews { state | dragState <- Just <| DraggingNode { attrs | overLambdaNode = Nothing } }
      DragEdgeStart attrs -> updateStateViews
                                { state | dragState <- Just <| DraggingEdge
                                    { attrs | upstreamNodes = upstreamNodes state.graph (fst attrs.fromPort) } }
      DragMove mousePos ->
          case state.dragState of
            Just (DraggingNode attrs) ->
                let newStateRes = moveNode state.graph attrs.nodePath (mousePos `pointSubtract` attrs.offset) |>
                                  R.map (\newGraph -> { state | graph <- newGraph })
                in case newStateRes of
                  Ok newState ->
                      case attrs.nodePath of
                       [x] -> newState
                       xs -> updateStateViews newState
                  Err msg -> Debug.crash msg
            Just (DraggingEdge attrs) ->
                { state | dragState <-
                            Just <| DraggingEdge { attrs | endPos <- mousePos } }
            Nothing -> state
            _ -> state
      DragEnd -> updateStateViews { state | dragState <- Nothing }
      -- add and remove
      AddNode posNode ->
          case addNode [] posNode state.graph of
            Ok newGraph -> updateStateViews { state | graph <- newGraph }
            Err msg -> Debug.crash msg
      RemoveNode nodePath ->
          case removeNode state.graph nodePath of
            Ok newGraph -> updateStateViews { state | graph <- newGraph }
            Err msg -> Debug.crash msg
      AddEdge edge ->
        case addEdge edge state.graph of
          Ok newGraph -> updateStateViews { state | graph <- newGraph
                                                  , dragState <- Nothing }
          Err msg -> Debug.crash msg
      RemoveEdge edge -> updateStateViews { state | graph <- removeEdge edge state.graph }
      -- drag into lambdas
      OverLambda lambdaPath ->
          case state.dragState of
            Just (DraggingNode attrs) ->
                let ds = state.dragState
                in updateStateViews { state | dragState <- Just <| DraggingNode { attrs | overLambdaNode <- Just lambdaPath } }
            _ -> Debug.crash "unexpected event"
      NotOverLambda lambdaPath ->
          case state.dragState of
            Just (DraggingNode attrs) ->
                let ds = state.dragState
                in updateStateViews { state | dragState <- Just <| DraggingNode { attrs | overLambdaNode <- Nothing } }
            _ -> Debug.crash "unexpected event"
      DropNodeInLambda {lambdaPath, droppedNodePath} -> state -- TODO


updateNodeViews : NodeDict -> State -> NodeDict
updateNodeViews nodeDict state =
    D.map (\id posNode -> { posNode | cachedDiagram <- viewNode posNode.node [id] state }) nodeDict

updateGraphViews : Graph -> State -> Graph
updateGraphViews graph state =
    { graph | nodes <- updateNodeViews graph.nodes state }

updateStateViews : State -> State
updateStateViews state =
    { state | graph <- updateGraphViews state.graph state }