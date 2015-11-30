module GraphEditor.Model where

import Dict as D
import List as L
import Set
import Maybe
import Debug

import Diagrams.Wiring exposing (CollageLocation)

import Model exposing (..)
import Model.Graph exposing (..)
import Runtime.Model as RM
import Util exposing (..)

-- TODO: having one of these for the python code editor
-- and wrapping the two of them up in an ADT in model
-- would be nice
type alias GraphViewModel =
    { mod : Module
    , currentName : FuncName -- TODO: should prob just be name & graph
    , currentGraph : Graph
    , collageLoc : CollageLocation
    , editorState : GraphEditorState
    , mode : GraphViewModeDenorm
    }

type GraphViewModeDenorm
    = EditingModeDenorm
    | ViewingRunModeDenorm RunId Run

isReadOnly : GraphViewModel -> Bool
isReadOnly viewModel =
    case viewModel.mode of
      EditingModeDenorm -> False
      _ -> True

makeViewModel : State -> GraphViewModel
makeViewModel state =
    case state.viewState of
      ViewingGraph attrs ->
          let mod =
                case attrs.mode of
                  EditingMode -> state.mod
                  ViewingRunMode runId ->
                      state |> getRunOrCrash runId |> .mod
          in  { mod = mod
              , currentName = attrs.name
              , currentGraph =
                    mod.userFuncs
                      |> D.get attrs.name
                      |> getMaybeOrCrash "no such user func"
                      |> (\func ->
                        case func of
                          UserFunc attrs -> attrs.graph
                          _ -> Debug.crash "only expected UserFuncs")
              , collageLoc = state.collageLoc
              , editorState = attrs.editorState
              , mode =
                  case attrs.mode of
                    EditingMode -> EditingModeDenorm
                    ViewingRunMode runId ->
                        ViewingRunModeDenorm runId (getRunOrCrash runId state)
              }
      EditingPythonFunc _ -> Debug.crash "expecting graph viewing state"

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
    if funcOutPortUsed viewModel.currentGraph thisNodePath then
      InvalidPort
    else
      case viewModel.editorState.mouseInteractionState of
        Just (Dragging (DraggingEdge attrs)) ->
            let (fromNodePath, _) = attrs.fromPort
            in
              if thisNodePath `startsWith` fromNodePath then
                -- dragging from this node
                InvalidPort
              else if inPortTaken viewModel.currentGraph (thisNodePath, slotId) then
                -- this node already taken
                TakenPort
              else if thisNodePath `Set.member` attrs.upstreamNodes then
                -- no cycles
                InvalidPort
              else if L.any (\unPath -> thisNodePath `startsWith` unPath) (Set.toList <| attrs.upstreamNodes) then
                -- can't go from in lambda to out
                InvalidPort
              else if goingUpTree fromNodePath thisNodePath then
                InvalidPort
              else
                -- TODO: wrong type!
                ValidPort
        _ -> NormalPort

-- TODO: highlight as valid when you mouse over an in port of same type
outPortState : GraphViewModel -> OutPortId -> PortState
outPortState viewModel (nodePath, slotId) =
    if funcOutPortUsed viewModel.currentGraph nodePath then
      case slotId of
        FuncValueSlot -> NormalPort
        _ -> InvalidPort
    else if anyNormalPortsUsed viewModel.currentGraph nodePath then
      case slotId of
        FuncValueSlot -> InvalidPort
        _ -> NormalPort
    else
      NormalPort

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

updateGraphFun : (Graph -> Result String Graph) -> GraphViewModel -> GraphViewModel
updateGraphFun updateFun viewModel =
    let newGraph = updateFun viewModel.currentGraph |> getOrCrash
        newUdf = UserFunc { name = viewModel.currentName
                          , graph = newGraph
                          }
        newUserFuncs =
            viewModel.mod.userFuncs
              |> D.insert viewModel.currentName newUdf
        currentMod = viewModel.mod
        newModule = { currentMod | userFuncs = newUserFuncs }
    in { viewModel | currentGraph = newGraph
                   , mod = newModule }

getLambdaId : Graph -> (Graph, Int)
getLambdaId graph =
    let lid = graph.nextLambdaId
    in ( { graph | nextLambdaId = lid + 1 }
       , lid
       )

getApId : Graph -> (Graph, Int)
getApId graph =
    let aid = graph.nextApId
    in ( { graph | nextApId = aid + 1 }
       , aid
       )

-- Runtime

type NodeStatus
    = WaitingForInputs
    | Running RM.Record
    | Done { args : RM.Record, results : RM.Record }
    | FuncUsedAsValue

getNodeStatus : NodePath -> Graph -> Maybe RM.CallTree -> NodeStatus
getNodeStatus nodePath graph maybeTree =
    if usedAsValue nodePath graph
    then FuncUsedAsValue
    else case maybeTree of
          Nothing -> WaitingForInputs
          Just (RM.CallTree tree) ->
              case tree.results of
                Just results ->
                    Done { args = tree.args
                         , results = results
                         }
                Nothing ->
                    Running tree.args

getOutPortValue : OutPortId -> Graph -> Run -> Maybe RM.Value
getOutPortValue (nodePath, outSlotId) graph run =
    case nodePath of
      [nodeId] ->
          case outSlotId of
            FuncValueSlot ->
                if usedAsValue nodePath graph
                then Just RM.FunctionVal
                else Nothing
            ApResultSlot resultName ->
                run.callTree
                  |> (\(RM.CallTree tree) -> tree.children)
                  |> D.get nodeId
                  |> (\maybeCT -> Maybe.andThen maybeCT
                      (\(RM.CallTree treeAttrs) ->
                        -- TODO: use maybe.map or whatever
                        case treeAttrs.results of
                          Just results ->
                              results
                                |> D.get resultName
                                |> getMaybeOrCrash "getOutPortValue: no such slot"
                                |> Just
                          Nothing -> Nothing))
            IfResultSlot ->
              Debug.crash "TODO: probably make Ifs a function, not a special case node..."

      _ -> Nothing
