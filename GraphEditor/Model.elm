module GraphEditor.Model where

import Diagrams.Geom (Point)

import Dict as D
import List as L
import Result as R
import Maybe as M
import Debug

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
          | LambdaNode NodeDict

-- TODO: this attrs thing is awkward
type alias ApNodeAttrs = { title : String, params : List String, results : List String }

type alias Edge = { from : OutPortId, to : InPortId }

-- graph

type alias NodeDict = D.Dict NodeId PosNode
type alias Graph = { nodes : NodeDict, edges : List Edge }

emptyGraph = { nodes = D.empty, edges = [] }

-- app state

type DraggingState = DraggingNode { nodePath : NodePath, offset : Point } -- offset at lowest level
                   | DraggingEdge { fromPort : OutPortId, endPos : Point }

type alias State = { graph : Graph, dragState : Maybe DraggingState }

-- tags

type Tag = NodeIdT NodeId
         | TitleT
         | InPortT InSlotId
         | OutPortT OutSlotId
         | XOut
         | Canvas

type Action = DragNodeStart { nodePath : NodePath, offset : Point }
            | DragEdgeStart { fromPort : OutPortId, endPos : Point }
            | DragMove Point
            | DragEnd
            | RemoveNode NodePath
            | RemoveEdge Edge
            | AddEdge Edge
            | NoOp

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
                  LambdaNode subDict -> nestedPosNodeUpdate subDict xs updateFn
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
    let notInvolvingNode e = fst e.from /= nodePath && fst e.to /= nodePath
    in nestedPosNodeUpdate graph.nodes nodePath (always Nothing)
        |> R.map (\newNodes -> { graph | nodes <- newNodes
                                       , edges <- L.filter notInvolvingNode graph.edges })

-- queries

inPortTaken : Graph -> InPortId -> Bool
inPortTaken g inPort = L.any (\{from, to} -> to == inPort) g.edges

-- TODO(perf): these are same for duration of drag. could save somewhere.
inPortState : State -> InPortId -> PortState
inPortState state (nodePath, slotId) =
    case state.dragState of
      Nothing -> NormalPort
      Just (DraggingNode _) -> NormalPort
      Just (DraggingEdge attrs) -> let (fromNodePath, _) = attrs.fromPort
                                   in if -- dragging from this node
                                         | fromNodePath == nodePath -> InvalidPort
                                         -- this node already taken
                                         | inPortTaken state.graph (nodePath, slotId) -> TakenPort
                                         -- TODO: wrong type!
                                         | otherwise -> ValidPort

-- TODO: highlight as valid when you mouse over an in port of same type
outPortState : State -> OutPortId -> PortState
outPortState _ _ = NormalPort
