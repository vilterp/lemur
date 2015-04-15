module Codegen.TopSort where

import List as L
import Dict as D

import Debug

import GraphEditor.Model (..)

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
