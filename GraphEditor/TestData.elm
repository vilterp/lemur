module GraphEditor.TestData where

import Diagrams.Geom (Point)

import GraphEditor.Model (..)
import GraphEditor.View (..)

import Dict as D
import List as L
import Result as R

import Debug

makePosNode : { node : Node, pos : Point, id : String } -> PosNode
makePosNode attrs = { attrs | cachedDiagram = viewNode attrs.node [attrs.id] emptyState }

fooNode = ApNode { title = "Foo", params = ["InAasdfasdfsdafasdfs", "asdfs", "InB", "InC"], results = ["out1", "out2"] }
fooPosNode = makePosNode { node = fooNode, pos = (-300, 100), id = "foo" }

bazNode = ApNode { title = "Baz", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
bazPosNode = makePosNode { node = bazNode, pos = (100, -200), id = "baz" }

barNode = ApNode { title = "Bar", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
barPosNode = makePosNode { node = barNode, pos = (100, 100), id = "bar" }

fooBarEdge = { from = (["foo"], ApResultSlot "out1"), to = (["bar"], ApParamSlot "InA") }
--fooBazEdge = { from = ("foo", "out2"), to = ("baz", "InC") }

subBazNode = ApNode { title = "SubBaz", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
subBazPosNode = makePosNode { node = subBazNode, pos = (-50, -50), id = "baz1" }

subBarNode = ApNode { title = "SubBar", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
subBarPosNode = makePosNode { node = subBarNode, pos = (50, 50), id = "bar1" }

subBarSubBazEdge = { from = (["lambda", "bar1"], ApResultSlot "out1"), to = (["lambda", "baz1"], ApParamSlot "InA") }

lambdaNode =
    LambdaNode
      { nodes = (D.fromList [ (subBarPosNode.id, subBarPosNode), (subBazPosNode.id, subBazPosNode) ])
      , dims = { width = 300, height = 200 }
      }
lambdaPosNode = makePosNode { node = lambdaNode, pos = (-450, -100), id = "lambda" }

ifNode = IfNode
ifPosNode = makePosNode { node = ifNode, pos = (-200, 300), id = "if1" }

nodes = [fooPosNode, bazPosNode, barPosNode, ifPosNode, lambdaPosNode]
edges = [fooBarEdge, subBarSubBazEdge]

initGraph : Graph
initGraph = let withNodes : Result String Graph
                withNodes = L.foldl (\posNode graphRes -> graphRes `R.andThen` (addNode [posNode.id] posNode))
                                    (Ok emptyGraph) nodes
                withEdges : Result String Graph
                withEdges = L.foldl (\edge graphRes -> graphRes `R.andThen` (addEdge edge))
                                    withNodes edges
            in case withEdges of
                 Ok graph -> graph
                 Err msg -> Debug.crash msg

initState : State
initState = { graph = initGraph, dragState = Nothing }
