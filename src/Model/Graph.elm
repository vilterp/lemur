module Model.Graph where

import Result as R
import Dict as D
import List as L
import Maybe as M
import Set
import String as S

import Diagrams.Geom exposing (Point)

import Model exposing (..)
import Util exposing (..)

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
                        |> R.map (\newNodes -> {posNode | node = LambdaNode { nodes = newNodes, dims = dims }})
                        |> R.map (\newLn -> D.update x (M.map (\_ -> newLn)) dict)
                  _ -> Err "invalid path: not a lambda node"
            Nothing -> Err "invalid path"

moveNode : NodePath -> Point -> Graph -> Result String Graph
moveNode nodePath newPos graph =
    nestedPosNodeUpdate graph.nodes nodePath (M.map (\posNode -> { posNode | pos = newPos }))
      |> R.map (\newNodes -> { graph | nodes = newNodes })

-- TODO: check dups...
addEdge : Edge -> Graph -> Result String Graph
addEdge newEdge graph = Ok { graph | edges = newEdge :: graph.edges }

removeEdge : Edge -> Graph -> Result String Graph
removeEdge edge graph = Ok { graph | edges = L.filter (\e -> e /= edge) graph.edges }

-- TODO: I think first arg should really be pathAbove, but don't feel like
-- refactoring. Need Elm IDE!
addNode : NodePath -> PosNode -> Graph -> Result String Graph
addNode fullPath posNode graph =
    nestedPosNodeUpdate graph.nodes fullPath (always <| Just posNode)
      |> R.map (\newNodes -> { graph | nodes = newNodes })

-- TODO: this silently fails with an invalid path, which is not great.
removeNode : NodePath -> Graph -> Result String Graph
removeNode nodePath graph =
    let involvingNode e = fst e.from `startsWith` nodePath || fst e.to `startsWith` nodePath
    in (nestedPosNodeUpdate graph.nodes nodePath (always Nothing)
            |> R.map (\newNodes -> { graph | nodes = newNodes
                                           , edges = L.filter (not << involvingNode) graph.edges }))

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
          addNode (lambdaPath ++ droppedNodePath) { posNode | pos = posInLambda } newGraph))

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

usedAsValue : NodePath -> Graph -> Bool
usedAsValue nodePath graph =
    case edgesFrom graph nodePath of
      [edge] ->
        case edge.from of
          (_, FuncValueSlot) -> True
          _ -> False
      _ -> False

-- for codegen, there can be no free in or out ports.
-- free in and out ports are an invalid state.
-- TODO: annoyingly repetitive, again
freeInPorts : Module -> Graph -> NodePath -> List InPortId
freeInPorts mod graph pathAbove =
    let takenInPorts = L.map .to graph.edges
        getInPorts (nodeId, posNode) =
          let nodePath = pathAbove ++ [nodeId]
          in if usedAsValue nodePath graph
             then []
             else inSlots mod posNode.node
                    |> L.map (\slot -> (nodePath, slot))
        allInPorts =
            D.toList graph.nodes
              |> L.concatMap getInPorts
    in allInPorts |> L.filter (\ip -> not <| ip `L.member` takenInPorts)

{-| KNOWN ISSUE: this can end up calling itself with same arguments,
if checking a recursive function. Need to either disallow recursion
or make functions explicitly declare param & return vals -}
freeOutPorts : Module -> Graph -> NodePath -> List OutPortId
freeOutPorts mod graph pathAbove =
    let takenOutPorts = L.map .from graph.edges -- can be dups, but that's ok
        getOutPorts (nodeId, posNode) =
          let nodePath = pathAbove ++ [nodeId]
          in if usedAsValue nodePath graph
             then []
             else outSlots mod posNode.node
                    |> L.map (\slot -> (nodePath, slot))
        allOutPorts =
            D.toList graph.nodes
              |> L.concatMap getOutPorts
    in allOutPorts |> L.filter (\ip -> not <| ip `L.member` takenOutPorts)

-- codegen support

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
          freeInPorts mod attrs.graph []
            |> L.map inPortToString

funcReturnVals : Module -> Func -> List String
funcReturnVals mod func =
    case func of
      BuiltinFunc attrs -> attrs.returnVals
      UserFunc attrs ->
          freeOutPorts mod attrs.graph []
            |> L.map outPortToString

