module GraphEditor.Model where

import Dict as D
import List as L
import Set
import Debug

import Diagrams.Wiring exposing (CollageLocation)

import Model exposing (..)
import Util exposing (..)

-- TODO: having one of these for the python code editor
-- and wrapping the two of them up in an ADT in model
-- would be nice
type alias GraphViewModel =
    { mod : Module
    , currentName : FuncName
    , currentGraph : Graph
    , collageLoc : CollageLocation
    , editorState : GraphEditorState
    , mode : GraphViewModeDenorm
    }

type GraphViewModeDenorm
    = EditingModeDenorm
    | ViewingRunModeDenorm RunId Run

makeViewModel : State -> GraphViewModel
makeViewModel state =
    case state.viewState of
      ViewingGraph attrs ->
          { mod = state.mod
          , currentName = attrs.name
          , currentGraph =
                state.mod.userFuncs
                  |> D.get attrs.name
                  |> getMaybeOrCrash "no such user func"
                  |> (\(UserFunc attrs) -> attrs.graph)
          , collageLoc = state.collageLoc
          , editorState = attrs.graphEditorState
          , mode =
              case attrs.mode of
                EditingMode -> EditingModeDenorm
                ViewingRunMode runId ->
                    ViewingRunModeDenorm runId (getRunOrCrash state runId)
          }
      EditingBuiltin _ -> Debug.crash "expecting graph viewing state"

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
inPortState : GraphViewModel -> InPortId -> PortState
inPortState viewModel (thisNodePath, slotId) =
    if funcOutPortUsed viewModel.currentGraph thisNodePath
    then InvalidPort
    else case viewModel.editorState.mouseInteractionState of
            Just (Dragging (DraggingEdge attrs)) ->
                let (fromNodePath, _) = attrs.fromPort
                in if -- dragging from this node
                      | thisNodePath `startsWith` fromNodePath -> InvalidPort
                      -- this node already taken
                      | inPortTaken viewModel.currentGraph (thisNodePath, slotId) -> TakenPort
                      -- no cycles
                      | thisNodePath `Set.member` attrs.upstreamNodes -> InvalidPort
                      | L.any (\unPath -> thisNodePath `startsWith` unPath) (Set.toList <| attrs.upstreamNodes) -> InvalidPort
                      -- can't go from in lambda to out
                      | goingUpTree fromNodePath thisNodePath -> InvalidPort
                      -- TODO: wrong type!
                      | otherwise -> ValidPort
            _ -> NormalPort

-- TODO: highlight as valid when you mouse over an in port of same type
outPortState : GraphViewModel -> OutPortId -> PortState
outPortState viewModel (nodePath, slotId) =
    if | funcOutPortUsed viewModel.currentGraph nodePath ->
            case slotId of
              FuncValueSlot -> NormalPort
              _ -> InvalidPort
       | anyNormalPortsUsed viewModel.currentGraph nodePath ->
            case slotId of
              FuncValueSlot -> InvalidPort
              _ -> NormalPort
       | otherwise -> NormalPort

-- TODO: use these in other queries
lambdaState : GraphViewModel -> NodePath -> LambdaState
lambdaState viewModel nodePath =
    case viewModel.editorState.mouseInteractionState of
      Just (Dragging (DraggingNode attrs)) ->
          case attrs.overLambdaNode of
            Just overLN ->
                if overLN == nodePath
                then if canBeDroppedInLambda viewModel.currentGraph overLN attrs.nodePath
                     then ValidNodeOverLS
                     else InvalidNodeOverLS
                else NormalLS
            Nothing -> NormalLS
      _ -> NormalLS

