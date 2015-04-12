module GraphEditor.Model where

import Diagrams.Geom (Point, Dims)
import Diagrams.Core (Diagram)

import Dict as D
import Set
import List as L
import Result as R
import Maybe as M
import Debug

import GraphEditor.Util (..)

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

type alias PosNode = { pos : Point, id : NodeId, node : Node
                     , cachedDiagram : Diagram Tag Action
                     }

-- TODO: more node types
type Node = ApNode ApNodeAttrs
          | IfNode
          | LambdaNode LambdaNodeAttrs

-- TODO: this attrs thing is awkward
type alias ApNodeAttrs = { title : String, params : List String, results : List String }
type alias LambdaNodeAttrs = { nodes : NodeDict, dims : Dims }

type alias Edge = { from : OutPortId, to : InPortId }

type LambdaState = NormalLS | ValidNodeOverLS | InvalidNodeOverLS

-- graph

type alias NodeDict = D.Dict NodeId PosNode
type alias Graph = { nodes : NodeDict, edges : List Edge }

emptyGraph = { nodes = D.empty, edges = [] }

-- app state

type DraggingState = DraggingNode { nodePath : NodePath, offset : Point, overLambdaNode : Maybe NodePath } -- offset at lowest level
                   | DraggingEdge { fromPort : OutPortId, endPos : Point, upstreamNodes : Set.Set NodePath }

type alias State = { graph : Graph, dragState : Maybe DraggingState }

emptyState = { graph = emptyGraph, dragState = Nothing }

-- tags

type Tag = NodeIdT NodeId
         | TitleT
         | InPortT InSlotId
         | OutPortT OutSlotId
         | XOut
         | Canvas

type Action
  -- dragging
  = DragNodeStart { nodePath : NodePath, offset : Point }
  | DragEdgeStart { fromPort : OutPortId, endPos : Point }
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
  | DropNodeInLambda { lambdaPath : NodePath, droppedNodePath : NodePath }

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

addNode : NodePath -> PosNode -> Graph -> Result String Graph
addNode pathAbove posNode graph =
    nestedPosNodeUpdate graph.nodes pathAbove (always <| Just posNode)
      |> R.map (\newNodes -> { graph | nodes <- newNodes })

-- TODO: this silently fails with an invalid path, which is not great.
removeNode : Graph -> NodePath -> Result String Graph
removeNode graph nodePath =
    let involvingNode e = fst e.from `startsWith` nodePath || fst e.to `startsWith` nodePath
    in (nestedPosNodeUpdate graph.nodes nodePath (always Nothing)
            |> R.map (\newNodes -> { graph | nodes <- newNodes
                                           , edges <- L.filter (not << involvingNode) graph.edges }))

-- queries

inPortTaken : Graph -> InPortId -> Bool
inPortTaken g inPort = L.any (\{from, to} -> to == inPort) g.edges

-- TODO(perf): these are same for duration of drag. could save somewhere.
inPortState : State -> InPortId -> PortState
inPortState state (nodePath, slotId) =
    if funcOutPortUsed state nodePath
    then InvalidPort
    else case state.dragState of
           Nothing -> NormalPort
           Just (DraggingNode _) -> NormalPort
           Just (DraggingEdge attrs) ->
              let (fromNodePath, _) = attrs.fromPort
              in if -- dragging from this node
                    | nodePath `startsWith` fromNodePath -> InvalidPort
                    -- this node already taken
                    | inPortTaken state.graph (nodePath, slotId) -> TakenPort
                    -- no cycles
                    | nodePath `Set.member` attrs.upstreamNodes -> InvalidPort
                    | L.any (\unPath -> nodePath `startsWith` unPath) (Set.toList <| attrs.upstreamNodes) -> InvalidPort
                    -- can't go from in lambda to out
                    | goingUpTree fromNodePath nodePath -> InvalidPort
                    -- TODO: wrong type!
                    | otherwise -> ValidPort

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
                           then if (L.isEmpty <| edgesFrom state.graph attrs.nodePath)
                                      && (L.isEmpty <| edgesTo state.graph attrs.nodePath)
                                then ValidNodeOverLS
                                else InvalidNodeOverLS
                           else NormalLS
            Nothing -> NormalLS
      _ -> NormalLS
