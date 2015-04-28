module Model where

import Diagrams.Geom exposing (Point, Dims)

import Dict as D
import Set
import List as L
import Result as R
import Maybe as M
import String as S

import Util exposing (..)

{-
type Action
    = ElemPanelAction ElementsPanel.Action
    | GraphEditorAction GraphEditor.Action
    | AddLambda
    | AddApNode Model.FuncId
    | NoOp

-- TODO: tabs, multiple modules
type alias State =
    { mod : Module
    , editingFn : Maybe FuncName
    , elemPanelState : ElemPanelState
    }

type alias ElemPanelState =
    { filter : Maybe String }

-}
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
    , nextApId : Int
    , nextLambdaId : Int
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
             , nextApId = 0
             , nextLambdaId = 0
             }

-- this mod is def getting crowded

nodePathToString : NodePath -> String
nodePathToString nodePath = S.join "_" nodePath

inSlotToString : InSlotId -> String
inSlotToString slotId =
    case slotId of
      ApParamSlot name -> name
      IfCondSlot -> "cond"
      IfTrueSlot -> "iftrue"
      IfFalseSlot -> "iffalse"

inPortToString : InPortId -> String
inPortToString (nodePath, slot) =
    (nodePathToString nodePath) ++ "_" ++ (inSlotToString slot)

outSlotToString : OutSlotId -> String
outSlotToString slotId =
    case slotId of
      ApResultSlot name -> "_" ++ name
      IfResultSlot -> "_result"
      FuncValueSlot -> ""

outPortToString : OutPortId -> String
outPortToString (nodePath, slot) =
    (nodePathToString nodePath) ++ (outSlotToString slot)

funcParams : Module -> Func -> List String
funcParams mod func =
    case func of
      BuiltinFunc attrs -> attrs.params
      UserFunc attrs ->
          freeInPorts mod attrs.graph
            |> L.map inPortToString

funcReturnVals : Module -> Func -> List String
funcReturnVals mod func =
    case func of
      BuiltinFunc attrs -> attrs.returnVals
      UserFunc attrs ->
          freeOutPorts mod attrs.graph
            |> L.map outPortToString

-- TODO: can't figure out how to use extensible records here
funcName : Func -> ModName
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

-- GRAPH (TODO: maybe factor this out?)

{- TODO: mark ports as results! (and as discarded?)
probably requires redefining in port and outport id
as ADTs
or NodeId as an ADT
or having a ResultsNode and ParamsNode (the janky way)
-}

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
type alias Graph = { nodes : NodeDict, edges : List Edge }

emptyGraph = { nodes = D.empty, edges = [] }

-- OPERATIONS

nestedPosNodeUpdate : NodeDict -> NodePath -> (Maybe PosNode -> Maybe PosNode) -> Result String NodeDict
nestedPosNodeUpdate dict path updateFn =
    case path of
      [] -> Err "invalid path: empty"
      [x] -> Ok <| D.update x updateFn dict
      (x::xs) ->
          case D.get x dict of
            Just posNode ->
                case posNode.node of
                  LambdaNode {nodes, dims} ->
                      nestedPosNodeUpdate nodes xs updateFn
                        |> R.map (\newNodes -> {posNode | node <- LambdaNode { nodes = newNodes, dims = dims }})
                        |> R.map (\newLn -> D.update x (M.map (\_ -> newLn)) dict)
                  _ -> Err "invalid path: not a lambda node"
            Nothing -> Err "invalid path"

moveNode : NodePath -> Point -> Graph -> Result String Graph
moveNode nodePath newPos graph =
    nestedPosNodeUpdate graph.nodes nodePath (M.map (\posNode -> { posNode | pos <- newPos }))
      |> R.map (\newNodes -> { graph | nodes <- newNodes })

-- TODO: check dups...
addEdge : Edge -> Graph -> Result String Graph
addEdge newEdge graph = Ok { graph | edges <- newEdge :: graph.edges }

removeEdge : Edge -> Graph -> Result String Graph
removeEdge edge graph = Ok { graph | edges <- L.filter (\e -> e /= edge) graph.edges }

-- TODO: I think first arg should really be pathAbove, but don't feel like
-- refactoring. Need Elm IDE!
addNode : NodePath -> PosNode -> Graph -> Result String Graph
addNode fullPath posNode graph =
    nestedPosNodeUpdate graph.nodes fullPath (always <| Just posNode)
      |> R.map (\newNodes -> { graph | nodes <- newNodes })

-- TODO: this silently fails with an invalid path, which is not great.
removeNode : NodePath -> Graph -> Result String Graph
removeNode nodePath graph =
    let involvingNode e = fst e.from `startsWith` nodePath || fst e.to `startsWith` nodePath
    in (nestedPosNodeUpdate graph.nodes nodePath (always Nothing)
            |> R.map (\newNodes -> { graph | nodes <- newNodes
                                           , edges <- L.filter (not << involvingNode) graph.edges }))

getNode : NodePath -> Graph -> Result String PosNode
getNode nodePath graph =
    let recurse nodeDict path =
          case path of
            [] -> Err "invalid path"
            [x] -> D.get x nodeDict |> R.fromMaybe ("not found:" ++ x)
            (x::xs) ->
                (D.get x nodeDict |> R.fromMaybe ("not found:" ++ x))
                  `R.andThen` (\posNode ->
                    case posNode.node of
                      LambdaNode {nodes, dims} ->
                          recurse nodes xs
                      _ -> Err <| x ++ " not a lambda node")
    in recurse graph.nodes nodePath

moveNodeToLambda : NodePath -> NodePath -> Point -> Graph -> Result String Graph
moveNodeToLambda lambdaPath droppedNodePath posInLambda graph =
    (getNode droppedNodePath graph)
      `R.andThen` (\posNode -> (removeNode droppedNodePath graph)
        `R.andThen` (\newGraph ->
          addNode (lambdaPath ++ droppedNodePath) { posNode | pos <- posInLambda } newGraph))

-- QUERIES

edgesFrom : Graph -> NodePath -> List Edge
edgesFrom graph nodePath =
    L.filter (\{from, to} -> fst from == nodePath) graph.edges

edgesTo : Graph -> NodePath -> List Edge
edgesTo graph nodePath =
    L.filter (\{from, to} -> fst to == nodePath) graph.edges

upstreamNodes : Graph -> NodePath -> Set.Set NodePath
upstreamNodes graph nodePath =
    let ofThisNode = L.map (fst << .from) (edgesTo graph nodePath)
        ofNodesOneUpstream = L.map (upstreamNodes graph) ofThisNode
    in multiUnion <| (Set.fromList ofThisNode) :: ofNodesOneUpstream

goingUpTree : NodePath -> NodePath -> Bool
goingUpTree fromPath toPath =
    L.length fromPath > L.length toPath

canBeDroppedInLambda : Graph -> NodePath -> NodePath -> Bool
canBeDroppedInLambda graph lambdaPath draggingPath =
    (L.isEmpty <| edgesFrom graph draggingPath)
        && (L.isEmpty <| edgesTo graph draggingPath)

-- TODO: use edgesFrom
inPortTaken : Graph -> InPortId -> Bool
inPortTaken g inPort = L.any (\{from, to} -> to == inPort) g.edges

-- TODO: use edgesFrom
funcOutPortUsed : Graph -> NodePath -> Bool
funcOutPortUsed graph nodePath =
    not <| L.isEmpty <|
      L.filter (\{from, to} -> fst from == nodePath && snd from == FuncValueSlot)
               graph.edges

-- TODO: use edgesTo & from
anyNormalPortsUsed : Graph -> NodePath -> Bool
anyNormalPortsUsed graph nodePath =
    not <| L.isEmpty <|
      L.filter (\{from, to} -> (fst from == nodePath && snd from /= FuncValueSlot) || (fst to == nodePath))
               graph.edges

-- in & out ports

inSlots : Module -> Node -> List InSlotId
inSlots mod node =
    case node of
      ApNode funcId ->
          getFuncOrCrash mod funcId
            |> funcParams mod
            |> L.map ApParamSlot
      IfNode -> [IfCondSlot, IfTrueSlot, IfFalseSlot]
      LambdaNode _ -> []

outSlots : Module -> Node -> List OutSlotId
outSlots mod node =
    case node of
      ApNode funcId ->
          getFuncOrCrash mod funcId
            |> funcReturnVals mod
            |> L.map ApResultSlot
      IfNode -> [IfResultSlot]
      LambdaNode _ -> [] -- never really want to return this

-- for codegen, there can be no free in or out ports.
-- free in and out ports are an invalid state.
-- TODO: annoyingly repetitive, again
freeInPorts : Module -> Graph -> List InPortId
freeInPorts mod graph =
    let takenInPorts = L.map .to graph.edges
        allInPorts =
            D.toList graph.nodes
              |> L.concatMap (\(nodeId, posNode) -> inSlots mod posNode.node
                                |> L.map (\slot -> ([nodeId], slot)))
    in allInPorts |> L.filter (\ip -> not <| ip `L.member` takenInPorts)

{-| KNOWN ISSUE: this can end up calling itself with same arguments,
if checking a recursive function. Need to either disallow recursion
or make functions explicitly declare param & return vals -}
freeOutPorts : Module -> Graph -> List OutPortId
freeOutPorts mod graph =
    let takenOutPorts = L.map .from graph.edges -- can be dups, but that's ok
        allOutPorts =
            D.toList graph.nodes
              |> L.concatMap (\(nodeId, posNode) ->
                    outSlots mod posNode.node |> L.map (\slot -> ([nodeId], slot)))
    in allOutPorts |> L.filter (\ip -> not <| ip `L.member` takenOutPorts)
