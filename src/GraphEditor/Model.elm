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

type DraggingState
    = DraggingNode { nodePath : NodePath, offset : Point, overLambdaNode : Maybe NodePath } -- offset at lowest level
    | DraggingEdge { fromPort : OutPortId, endPos : Point, upstreamNodes : Set.Set NodePath }
    | DragPanning { offset : Point }

type alias State =
    { mod : Module
    , funcName : FuncName
    , dragState : Maybe DraggingState
    , pan : Point
    }

initState : Module -> FuncName -> State
initState mod funcName =
    { mod = mod, funcName = funcName, dragState = Nothing, pan = (0, 0) }

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

type PortState
    = NormalPort
    | InvalidPort
    | TakenPort
    | ValidPort

type LambdaState
    = ValidNodeOverLS
    | InvalidNodeOverLS
    | NormalLS

getUserFunc : State -> UserFuncAttrs
getUserFunc state =
    case state.mod.userFuncs |> D.get state.funcName of
      Just (UserFunc attrs) -> attrs
      Just (BuiltinFunc _) -> Debug.crash "builtin func supposed to be user func"
      Nothing -> Debug.crash <| "no func found named " ++ state.funcName

getGraph : State -> Graph
getGraph state =
    getUserFunc state |> .graph

updateGraph : State -> (Graph -> Result String Graph) -> State
updateGraph state updateFun =
    let mod = state.mod
        newUFs = state.mod.userFuncs
                  |> D.update state.funcName (\uFunc ->
                      M.map (\(UserFunc attrs) ->
                          UserFunc { attrs | graph <- updateFun attrs.graph |> getOrCrash }) uFunc)
    in { state | mod <- { mod | userFuncs <- newUFs } }

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

