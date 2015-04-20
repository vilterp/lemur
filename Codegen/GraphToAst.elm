module Codegen.GraphToAst where

import List as L
import Dict as D
import String

import Debug

import GraphEditor.Model (..)
import Codegen.AST as AST

noDependencies : Graph -> List (NodePath, Node)
noDependencies graph =
    D.filter (\k v -> edgesTo graph [k] |> L.isEmpty) graph.nodes
      |> D.toList
      |> L.map (\(k, posNode) -> ([k], posNode.node))

topSort : Graph -> List (NodePath, Node)
topSort graph =
    let recurse newGraph list =
          case noDependencies newGraph of
            [] -> list
            (pathAndNode::_) ->
                case removeNode newGraph (fst pathAndNode) of
                  Ok newerGraph -> recurse newerGraph (pathAndNode::list)
                  Err msg -> Debug.crash msg
    in recurse graph [] |> L.reverse

nodePathToString : NodePath -> String
nodePathToString nodePath = String.join "_" nodePath

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
      ApResultSlot name -> name
      IfResultSlot -> "result"
      FuncValueSlot -> "func"

-- TODO factor out to nodePathToString
outPortToString : OutPortId -> String
outPortToString (nodePath, slot) =
    (nodePathToString nodePath) ++ "_" ++ (outSlotToString slot)

getSrcPort : Graph -> InPortId -> Maybe OutPortId
getSrcPort graph (nodePath, slotId) =
    case edgesTo graph nodePath |> L.filter (\{from, to} -> snd to == slotId) of
      [] -> Nothing
      [{from, to}] -> Just from
      _ -> Debug.crash "should only be one edge to a port"

nodeToStmt : Graph -> (NodePath, Node) -> AST.Statement
nodeToStmt graph (nodePath, node) =
    case node of
      ApNode {title, params, results} ->
          -- TODO: these don't necessarily line up, & some of them need
          -- to be from params to this func
          let toEdges = edgesTo graph nodePath
              getSrcVar : InPortId -> String
              getSrcVar inPortId =
                  case getSrcPort graph inPortId of
                    Just outPort -> outPortToString outPort
                    Nothing -> inPortToString inPortId
              toVars = L.map getSrcVar <| L.map (\inSlot -> (nodePath, ApParamSlot inSlot)) params
              call = AST.FuncCall { func = AST.Variable title
                                  , args = L.map AST.Variable toVars
                                  }
          in AST.VarAssn { varName = (nodePathToString nodePath) ++ "_result"
                         , expr = call
                         }
      IfNode ->
          -- TODO: 4 real
          AST.IfStmt { cond = AST.IntLit 2
                     , ifBlock = []
                     , elseBlock = []
                     }
      LambdaNode attrs ->
          -- TODO: 4 real
          AST.FuncDef { name = String.join "_" nodePath
                      , args = []
                      , body = []
                      }

-- TODO: this will always be a FuncDef, not just any statement
toAst : String -> Graph -> AST.Statement
toAst funcName graph =
    let nodeList : List (NodePath, Node)
        nodeList = topSort graph
    in AST.FuncDef { name = funcName
                   , args = L.map inPortToString <| freeInPorts graph
                   , body = L.map (nodeToStmt graph) nodeList
                   }
