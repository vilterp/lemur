module Codegen where

import List as L
import Dict as D
import String as S

import Debug

import Model exposing (..)
import Codegen.AST as AST
import Codegen.PrettyPrint as PrettyPrint

import Util exposing (..)

noDependencies : Graph -> List (NodePath, Node)
noDependencies graph =
    D.filter (\k v -> edgesTo graph [k] |> L.isEmpty) graph.nodes
      |> D.toList
      |> L.map (\(k, posNode) -> ([k], posNode.node))

-- BUG: doesn't seem to get all connected components (?)
-- don't see why tho
topSort : Graph -> List (NodePath, Node)
topSort graph =
    let recurse newGraph list =
          case noDependencies newGraph of
            [] -> list
            (pathAndNode::_) ->
                case removeNode (fst pathAndNode) newGraph of
                  Ok newerGraph -> recurse newerGraph (pathAndNode::list)
                  Err msg -> Debug.crash msg
    in recurse graph [] |> L.reverse

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

-- TODO factor out to nodePathToString
outPortToString : OutPortId -> String
outPortToString (nodePath, slot) =
    (nodePathToString nodePath) ++ (outSlotToString slot)

getSrcPort : Graph -> InPortId -> Maybe OutPortId
getSrcPort graph (nodePath, slotId) =
    case edgesTo graph nodePath |> L.filter (\{from, to} -> snd to == slotId) of
      [] -> Nothing
      [{from, to}] -> Just from
      _ -> Debug.crash "should only be one edge to a port"

nodeToStmt : Module -> Graph -> (NodePath, Node) -> AST.Statement
nodeToStmt mod graph (nodePath, node) =
    case node of
      ApNode funcId ->
          -- TODO: these don't necessarily line up, & some of them need
          -- to be from params to this func
          let func = getFuncOrCrash mod funcId
              toEdges = edgesTo graph nodePath
              getSrcVar : InPortId -> String
              getSrcVar inPortId =
                  case getSrcPort graph inPortId of
                    Just outPort -> outPortToString outPort
                    Nothing -> inPortToString inPortId
              toVars = L.map getSrcVar <| L.map (\inSlot -> (nodePath, ApParamSlot inSlot)) (func |> funcParams)
              call = AST.FuncCall { func = AST.Variable (func |> funcName)
                                  , args = L.map AST.Variable toVars
                                  }
          in AST.VarAssn { varName = (nodePathToString nodePath)
                         , expr = call
                         }
      IfNode ->
          -- TODO: 4 real
          -- since we're thinking of this as an expression, should assign 
          -- variable for its result
          -- and then this function will need to return a list of statements, not just one
          AST.IfStmt { cond = AST.IntLit 2
                     , ifBlock = []
                     , elseBlock = []
                     }
      LambdaNode attrs ->
          -- TODO: 4 real
          AST.FuncDef { name = S.join "_" nodePath
                      , args = []
                      , body = []
                      }

makeReturnStmt : Module -> Graph -> AST.Statement
makeReturnStmt mod graph =
    case freeOutPorts mod graph of
      [op] ->
          outPortToString op |> AST.Variable |> AST.Return
      outPorts ->
          outPorts
            |> L.map (\op -> let var = outPortToString op
                             in (var, AST.Variable var))
            |> D.fromList
            |> AST.DictLiteral
            |> AST.Return

-- TODO: this will always be a FuncDef, not just any statement
userFuncToAst : Module -> UserFuncAttrs -> AST.Statement
userFuncToAst mod userFunc =
    let bodyStmts = topSort userFunc.graph |> L.map (nodeToStmt mod userFunc.graph)
        returnStmt = makeReturnStmt mod userFunc.graph
    in AST.FuncDef { name = userFunc.name
                   , args = L.map inPortToString <| freeInPorts mod userFunc.graph
                   , body = bodyStmts ++ [returnStmt]
                   }

funcToString : Module -> Model.Func -> String
funcToString mod func =
   case func of
     Model.BuiltinFunc attrs ->
         let heading = "def " ++ attrs.name ++
                          "(" ++ (S.join ", " attrs.params) ++ "):"
         in PrettyPrint.headerBlock heading
              [PrettyPrint.preIndented attrs.pythonCode]
            |> PrettyPrint.stringify
     Model.UserFunc attrs ->
         userFuncToAst mod attrs
           |> AST.statementToPython

moduleToPython : Module -> String
moduleToPython mod =
    D.union mod.userFuncs mod.builtinFuncs
      |> D.toList
      |> L.map (funcToString mod << snd)
      |> S.join "\n\n"
