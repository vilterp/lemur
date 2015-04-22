module GraphEditor.Model where

import Diagrams.Geom exposing (Point, Dims)

import Dict as D
import Set
import List as L
import Result as R
import Maybe as M
import Debug

import GraphEditor.Util exposing (..)

-- data structures

type DraggingState
    = DraggingNode { nodePath : NodePath, offset : Point, overLambdaNode : Maybe NodePath } -- offset at lowest level
    | DraggingEdge { fromPort : OutPortId, endPos : Point, upstreamNodes : Set.Set NodePath }
    | DragPanning { offset : Point }

type alias State =
    { graph : Graph
    , dragState : Maybe DraggingState
    , pan : Point
    }

emptyState = { graph = emptyGraph, dragState = Nothing, pan = (0, 0) }

-- tags

type Tag
    = TopLevel
    | NodeIdT NodeId
    | TitleT
    | InPortT InSlotId
    | OutPortT OutSlotId
    | XOut
    | Canvas

type Action
    -- dragging & panning
    = DragNodeStart { nodePath : NodePath, offset : Point }
    | DragEdgeStart { fromPort : OutPortId, endPos : Point }
    | PanStart { offset : Point }
    | DragMove Point
    | DragEnd
    -- add and remove
    | AddNode PosNode
    | RemoveNode NodePath
    | AddEdge Edge
    | RemoveEdge Edge
    -- dropping into lambdas
    | OverLambda NodePath
    | NotOverLambda NodePath
    | DropNodeInLambda { lambdaPath : NodePath, droppedNodePath : NodePath, posInLambda : Point }

-- operations

-- queries

-- TODO(perf): these are same for duration of drag. could save somewhere.
inPortState : State -> InPortId -> PortState
inPortState state (thisNodePath, slotId) =
    if funcOutPortUsed state thisNodePath
    then InvalidPort
    else case state.dragState of
           Just (DraggingEdge attrs) ->
              let (fromNodePath, _) = attrs.fromPort
              in if -- dragging from this node
                    | thisNodePath `startsWith` fromNodePath -> InvalidPort
                    -- this node already taken
                    | inPortTaken state.graph (thisNodePath, slotId) -> TakenPort
                    -- no cycles
                    | thisNodePath `Set.member` attrs.upstreamNodes -> InvalidPort
                    | L.any (\unPath -> thisNodePath `startsWith` unPath) (Set.toList <| attrs.upstreamNodes) -> InvalidPort
                    -- can't go from in lambda to out
                    | goingUpTree fromNodePath thisNodePath -> InvalidPort
                    -- TODO: wrong type!
                    | otherwise -> ValidPort
           _ -> NormalPort

-- TODO: highlight as valid when you mouse over an in port of same type
outPortState : State -> OutPortId -> PortState
outPortState state (nodePath, slotId) =
    if | funcOutPortUsed state nodePath ->
            case slotId of
              FuncValueSlot -> NormalPort
              _ -> InvalidPort
       | anyNormalPortsUsed state nodePath ->
            case slotId of
              FuncValueSlot -> InvalidPort
              _ -> NormalPort
       | otherwise -> NormalPort

-- TODO: use these in other queries
lambdaState : State -> NodePath -> LambdaState
lambdaState state nodePath =
    case state.dragState of
      Just (DraggingNode attrs) ->
          case attrs.overLambdaNode of
            Just overLN -> if overLN == nodePath
                           then if canBeDroppedInLambda state.graph overLN attrs.nodePath
                                then ValidNodeOverLS
                                else InvalidNodeOverLS
                           else NormalLS
            Nothing -> NormalLS
      _ -> NormalLS

