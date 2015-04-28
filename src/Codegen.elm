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

getSrcPort : Graph -> InPortId -> Maybe OutPortId
getSrcPort graph (nodePath, slotId) =
    case edgesTo graph nodePath |> L.filter (\{from, to} -> snd to == slotId) of
      [] -> Nothing
      [{from, to}] -> Just from
      _ -> Debug.crash "should only be one edge to a port"

nodeToStmt : Module -> Graph -> (NodePath, Node) -> List AST.Statement
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
              paramVars = L.map getSrcVar <| L.map (\inSlot -> (nodePath, ApParamSlot inSlot))
                                                   (func |> funcParams mod)
              call = AST.FuncCall { func = AST.Variable (func |> funcName)
                                  , args = L.map AST.Variable paramVars
                                  }
              resultVarName = nodePathToString nodePath
              callAssn = AST.VarAssn { varName = resultVarName
                                     , expr = call
                                     }
              -- now make vars for return values
              resultVars =
                func
                  |> funcReturnVals mod
                  |> L.map (\name ->
                      AST.VarAssn { varName = outPortToString (nodePath, ApResultSlot name)
                                  , expr = AST.DictAccess
                                              (AST.Variable resultVarName)
                                              name
                                  })
          in callAssn :: resultVars
      IfNode ->
          {- TODO: 4 real
          since we're thinking of this as an expression, should assign 
          variable for its result
          and then this function will need to return a list of statements, not just one.
           -}
          [ AST.IfStmt { cond = AST.IntLit 2
                       , ifBlock = []
                       , elseBlock = []
                       }
          ]
      LambdaNode attrs ->
          -- TODO: 4 real
          -- first basic, then closures
          -- TODO: factor out common stuff w/ userFuncToAst
          let subGraph =
                { nodes = attrs.nodes
                , edges = graph.edges
                }
              bodyStmts = topSort subGraph
                            |> L.map (\(path, node) -> (nodePath ++ path, node))
                            |> L.concatMap (nodeToStmt mod subGraph)
              returnStmt = makeReturnStmt mod subGraph nodePath
              np = Debug.log "nodePath" nodePath
          in [ AST.FuncDef { name = S.join "_" nodePath
                           , args = freeInPorts mod subGraph nodePath
                                      |> L.map inPortToString
                           , body = bodyStmts ++ [returnStmt]
                           }
             ]

makeReturnStmt : Module -> Graph -> NodePath -> AST.Statement
makeReturnStmt mod graph nodePath =
    freeOutPorts mod graph nodePath
      |> L.map (\op -> let var = outPortToString op
                       in (var, AST.Variable var))
      |> D.fromList
      |> AST.DictLiteral
      |> AST.Return

-- TODO: this will always be a FuncDef, not just any statement
userFuncToAst : Module -> UserFuncAttrs -> AST.Statement
userFuncToAst mod userFunc =
    let bodyStmts = topSort userFunc.graph
                      |> L.concatMap (nodeToStmt mod userFunc.graph)
        returnStmt = makeReturnStmt mod userFunc.graph []
    in AST.FuncDef { name = userFunc.name
                   , args = freeInPorts mod userFunc.graph []
                              |> L.map inPortToString
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
