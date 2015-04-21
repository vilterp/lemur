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

type PortState = ValidPort
               | InvalidPort
               | TakenPort
               | NormalPort

-- TODO: abstract out diagram caching (...)
type alias PosNode = { pos : Point, id : NodeId, node : Node }

-- TODO: more node types
type Node = ApNode ApNodeAttrs
          | IfNode
          | LambdaNode LambdaNodeAttrs

type alias ApNodeAttrs = { title : String, params : List String, results : List String }
type alias LambdaNodeAttrs = { nodes : NodeDict, dims : Dims }

emptyLambdaNode =
    LambdaNode { nodes = D.empty, dims = { width = 200, height = 200 } }

-- TODO: should prob be src & dst
type alias Edge = { from : OutPortId, to : InPortId }

type LambdaState = NormalLS | ValidNodeOverLS | InvalidNodeOverLS

-- graph

type alias NodeDict = D.Dict NodeId PosNode
type alias Graph = { nodes : NodeDict, edges : List Edge }

emptyGraph = { nodes = D.empty, edges = [] }

-- app state

type DraggingState = DraggingNode { nodePath : NodePath, offset : Point, overLambdaNode : Maybe NodePath } -- offset at lowest level
                   | DraggingEdge { fromPort : OutPortId, endPos : Point, upstreamNodes : Set.Set NodePath }
                   | DragPanning { offset : Point }

type alias State = { graph : Graph, dragState : Maybe DraggingState, pan : Point }

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

moveNode : Graph -> NodePath -> Point -> Result String Graph
moveNode graph nodePath newPos =
    nestedPosNodeUpdate graph.nodes nodePath (M.map (\posNode -> { posNode | pos <- newPos }))
      |> R.map (\newNodes -> { graph | nodes <- newNodes })

-- TODO: check dups...
addEdge : Edge -> Graph -> Result String Graph
addEdge newEdge graph = Ok { graph | edges <- newEdge :: graph.edges }

removeEdge : Edge -> Graph -> Graph
removeEdge edge graph = { graph | edges <- L.filter (\e -> e /= edge) graph.edges }

-- TODO: I think first arg should really be pathAbove, but don't feel like
-- refactoring. Need Elm IDE!
addNode : NodePath -> PosNode -> Graph -> Result String Graph
addNode fullPath posNode graph =
    nestedPosNodeUpdate graph.nodes fullPath (always <| Just posNode)
      |> R.map (\newNodes -> { graph | nodes <- newNodes })

-- TODO: this silently fails with an invalid path, which is not great.
removeNode : Graph -> NodePath -> Result String Graph
removeNode graph nodePath =
    let involvingNode e = fst e.from `startsWith` nodePath || fst e.to `startsWith` nodePath
    in (nestedPosNodeUpdate graph.nodes nodePath (always Nothing)
            |> R.map (\newNodes -> { graph | nodes <- newNodes
                                           , edges <- L.filter (not << involvingNode) graph.edges }))

getNode : Graph -> NodePath -> Result String PosNode
getNode graph nodePath =
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

moveNodeToLambda : Graph -> NodePath -> NodePath -> Point -> Result String Graph
moveNodeToLambda graph lambdaPath droppedNodePath posInLambda =
    (getNode graph droppedNodePath)
      `R.andThen` (\posNode -> (removeNode graph droppedNodePath)
        `R.andThen` (\newGraph ->
          addNode (lambdaPath ++ droppedNodePath) { posNode | pos <- posInLambda } newGraph))

-- queries

inPortTaken : Graph -> InPortId -> Bool
inPortTaken g inPort = L.any (\{from, to} -> to == inPort) g.edges

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

funcOutPortUsed : State -> NodePath -> Bool
funcOutPortUsed state nodePath =
    not <| L.isEmpty <|
      L.filter (\{from, to} -> fst from == nodePath && snd from == FuncValueSlot)
               state.graph.edges

anyNormalPortsUsed : State -> NodePath -> Bool
anyNormalPortsUsed state nodePath =
    not <| L.isEmpty <|
      L.filter (\{from, to} -> (fst from == nodePath && snd from /= FuncValueSlot) || (fst to == nodePath))
               state.graph.edges

-- TODO: use these in other queries
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

canBeDroppedInLambda : Graph -> NodePath -> NodePath -> Bool
canBeDroppedInLambda graph lambdaPath draggingPath =
    (L.isEmpty <| edgesFrom graph draggingPath)
        && (L.isEmpty <| edgesTo graph draggingPath)

inSlots : Node -> List InSlotId
inSlots node =
    case node of
      ApNode attrs -> L.map ApParamSlot attrs.params
      IfNode -> [IfCondSlot, IfTrueSlot, IfFalseSlot]
      LambdaNode _ -> []

outSlots : Node -> List OutSlotId
outSlots node =
    case node of
      ApNode attrs -> L.map ApResultSlot attrs.results
      IfNode -> [IfResultSlot]
      LambdaNode _ -> [] -- never really want to return this

-- TODO: annoyingly repetitive, again
freeInPorts : Graph -> List InPortId
freeInPorts graph =
    let takenInPorts = L.map .to graph.edges
        allInPorts =
            D.toList graph.nodes
              |> L.concatMap (\(nodeId, posNode) -> inSlots posNode.node
                                |> L.map (\slot -> ([nodeId], slot)))
    in allInPorts |> L.filter (\ip -> not <| ip `L.member` takenInPorts)

freeOutPorts : Graph -> List OutPortId
freeOutPorts graph =
    let takenOutPorts = L.map .from graph.edges -- can be dups, but that's ok
        allOutPorts =
            D.toList graph.nodes
              |> L.concatMap (\(nodeId, posNode) -> outSlots posNode.node
                                |> L.map (\slot -> ([nodeId], slot)))
    in allOutPorts |> L.filter (\ip -> not <| ip `L.member` takenOutPorts)
