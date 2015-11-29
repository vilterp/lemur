module Model where

import Dict as D
import Set
import List as L
import Result as R
import Maybe as M
import String as S
import Debug

import Diagrams.Geom exposing (Point, Dims)
import Diagrams.Wiring exposing (CollageLocation, PrimMouseEvent)
import Diagrams.Core as DC
import Diagrams.Interact as DI
import Http

-- hope this doesn't become circular
import Runtime.Model

import Util exposing (..)

-- top-level state
type alias State =
    { mod : Module
    , elemPanelFilter : String
    , collageLoc : CollageLocation
    , viewState : ViewState
    , nextRunId : RunId
    , runs : D.Dict RunId Run
    }

type ViewState
    = ViewingGraph
        { name : FuncName
        , editorState : GraphEditorState
        , mode : GraphViewMode
        }
    | EditingBuiltin { name : FuncName }

type GraphViewMode
    = EditingMode
    | ViewingRunMode RunId

{-
Need to be able to do three things:
- process updates (i.e. edit the current graph)
- view the graph
- compute the actions for each part of the graph
-}

-- Actions

type Action
    -- editor stuff
    = FilterElemPanel String
    | OpenUDF FuncName
    | OpenBuiltin FuncName
    | OpenRun RunId
    -- graph ops
    | GraphAction GraphAction
    | CanvasMouseEvt (CollageLocation, PrimMouseEvent)
    -- running
    | RunCode CodeReq
    | RunCodeError Http.Error
    | ExecutionUpdates RunId (List Runtime.Model.ExecutionUpdate)
    --
    | NoOp

type GraphEditorAction
    = InternalAction GraphEditorInternalAction
    | ExternalAction GraphAction

type GraphAction
    = MoveNode NodePath Point
    | AddLambda
    | AddApNode FuncId
    | RemoveNode NodePath
    | AddEdge Edge
    | RemoveEdge Edge
    | DropNodeInLambda { lambdaPath : NodePath, droppedNodePath : NodePath, posInLambda : Point }

type GraphEditorInternalAction
    = DragNodeStart { nodePath : NodePath, offset : Point }
    | DragEdgeStart { fromPort : OutPortId, endPos : Point }
    | PanStart { offset : Point }
    | DragEdgeTo Point
    | PanTo Point
    | DragEnd
    -- dropping into lambdas
    | OverLambda NodePath
    | NotOverLambda NodePath
    -- viewing types & values at ports
    | OverInPort InPortId
    | OverOutPort OutPortId
    | NotOverPort

-- MODULE

type alias ModName = String
type alias FuncName = String
type alias FuncId = FuncName -- someday: within a module

-- assumes no duplicate names enforced by editor
type alias Module =
    { name : ModName
    , builtinFuncs : D.Dict String Func
    , userFuncs : D.Dict String Func
    }

-- TODO: can't figure out rn how to use extensible records here
type Func
    -- TODO: this should be called textual or something, not builtin.
    -- these are also "user defined"
    = BuiltinFunc BuiltinFuncAttrs
    | UserFunc UserFuncAttrs

type alias BuiltinFuncAttrs =
    { name : String
    , params : List String
    , returnVals : List String
    , pythonCode : String
    }

type alias UserFuncAttrs =
    { name : String
    , graph : Graph
    }

-- TODO: user func args are computed from graph

emptyBuiltinFunc : String -> Func
emptyBuiltinFunc name =
    BuiltinFunc { name = name
                , pythonCode = "return None"
                , params = []
                , returnVals = ["result"]
                }

emptyUserFunc : FuncName -> Func
emptyUserFunc name =
    UserFunc { name = name
             , graph = emptyGraph
             }

-- TODO: can't figure out how to use extensible records here
funcName : Func -> FuncName
funcName func =
    case func of
      UserFunc attrs -> attrs.name
      BuiltinFunc attrs -> attrs.name

-- assumes no duplicate names
getFunc : Module -> FuncName -> Maybe Func
getFunc mod name =
    case D.get name mod.builtinFuncs of
      Just bif -> Just bif
      Nothing -> D.get name mod.userFuncs

getFuncOrCrash : Module -> FuncName -> Func
getFuncOrCrash mod funcName =
    getFunc mod funcName
      |> getMaybeOrCrash ("no such function " ++ funcName)

getRunOrCrash : RunId -> State -> Run
getRunOrCrash runId state =
    state.runs
      |> D.get runId
      |> getMaybeOrCrash "no such run found"

-- GRAPH

type alias NodeId = String
type alias NodePath = List NodeId

type OutSlotId = ApResultSlot String
               | IfResultSlot
               | FuncValueSlot

type InSlotId = ApParamSlot String
              | IfCondSlot
              | IfTrueSlot
              | IfFalseSlot

type alias OutPortId = (NodePath, OutSlotId)
type alias InPortId = (NodePath, InSlotId)

type alias PosNode = { pos : Point, id : NodeId, node : Node }

type Node = ApNode FuncId
          | IfNode
          | LambdaNode LambdaNodeAttrs

type alias LambdaNodeAttrs = { nodes : NodeDict, dims : Dims }

emptyLambdaNode =
    LambdaNode { nodes = D.empty, dims = { width = 200, height = 200 } }

type alias Edge = { from : OutPortId, to : InPortId }

type alias NodeDict = D.Dict NodeId PosNode
type alias Graph =
    { nodes : NodeDict
    , edges : List Edge
    , nextApId : Int
    , nextLambdaId : Int
    }

emptyGraph =
    { nodes = D.empty
    , edges = []
    , nextApId = 0
    , nextLambdaId = 0
    }

type alias CodeReq =
    { runId : RunId
    , mod : Module
    , mainName : FuncName
    }

-- GRAPH EDITOR (TODO: figure out how to move this to its own module)

type alias GraphEditorState =
    { diagram : DC.Diagram Tag GraphEditorAction
    , mouseState : DI.MouseState Tag GraphEditorAction
    , mouseInteractionState : Maybe MouseInteractionState
    , pan : Point
    }

type MouseInteractionState
    = Dragging DraggingState
    | HoveringInPort InPortId
    | HoveringOutPort OutPortId

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

-- RUNS

type alias RunId = Int
type alias Run =
    { userFuncName : FuncName
    , mod : Module
    , callTree : Runtime.Model.CallTree
    }

addNewRun : FuncName -> State -> State
addNewRun funcName state =
    let rid = state.nextRunId
        newRun = { userFuncName = funcName
                 , mod = state.mod
                 , callTree = Runtime.Model.emptyCallTree
                 }
    in { state | nextRunId = rid + 1
               , runs = D.insert rid newRun state.runs }

processExecutionUpdate : RunId -> Runtime.Model.ExecutionUpdate -> State -> State
processExecutionUpdate runId update state =
    let updateFun maybeRun =
          case maybeRun of
            Just run ->
                let newCallTree = run.callTree |> Runtime.Model.processUpdate update
                in Just <| { run | callTree = newCallTree }
            Nothing -> Debug.crash "received update for nonexistent run"
    in { state | runs = D.update runId updateFun state.runs }
