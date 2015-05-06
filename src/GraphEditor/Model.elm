module GraphEditor.Model where

import Diagrams.Geom exposing (Point, Dims)

import Dict as D
import Set
import List as L
import Result as R
import Maybe as M
import Debug

import Model exposing (..)

import Util exposing (..)

-- data structures

type alias State =
    { diagram : D.Diagram Tag Action
    , mouseState : DI.MouseState Tag Action
    , collageDims : DG.Dims
    , dragState : Maybe DraggingState
    , pan : Point
    }

type DraggingState
    = DraggingNode { nodePath : NodePath, offset : Point, overLambdaNode : Maybe NodePath } -- offset at lowest level
    | DraggingEdge { fromPort : OutPortId, endPos : Point, upstreamNodes : Set.Set NodePath }
    | DragPanning { offset : Point }

-- tags

type Tag
    = TopLevel
    | NodeIdT NodeId
    | TitleT
    | InPortT InSlotId
    | OutPortT OutSlotId
    | XOut
    | Canvas

type PortState
    = NormalPort
    | InvalidPort
    | TakenPort
    | ValidPort

type LambdaState
    = ValidNodeOverLS
    | InvalidNodeOverLS
    | NormalLS

-- TODO(perf): these are same for duration of drag. could save somewhere.
inPortState : State -> InPortId -> PortState
inPortState state (thisNodePath, slotId) =
    if funcOutPortUsed (state |> getGraph) thisNodePath
    then InvalidPort
    else case state.dragState of
           Just (DraggingEdge attrs) ->
              let (fromNodePath, _) = attrs.fromPort
              in if -- dragging from this node
                    | thisNodePath `startsWith` fromNodePath -> InvalidPort
                    -- this node already taken
                    | inPortTaken (state |> getGraph) (thisNodePath, slotId) -> TakenPort
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
    if | funcOutPortUsed (state |> getGraph) nodePath ->
            case slotId of
              FuncValueSlot -> NormalPort
              _ -> InvalidPort
       | anyNormalPortsUsed (state |> getGraph) nodePath ->
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
                           then if canBeDroppedInLambda (state |> getGraph) overLN attrs.nodePath
                                then ValidNodeOverLS
                                else InvalidNodeOverLS
                           else NormalLS
            Nothing -> NormalLS
      _ -> NormalLS

