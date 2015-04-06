module GraphEditor.TestData where

import GraphEditor.Model (..)

import Dict as D
import List as L
import Result as R

import Debug

fooNode = ApNode { title = "Foo", params = ["InAasdfasdfsdafasdfs", "asdfs", "InB", "InC"], results = ["out1", "out2"] }
fooPosNode = { node = fooNode, pos = (-300, 100), id = "foo" }

bazNode = ApNode { title = "Baz", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
bazPosNode = { node = bazNode, pos = (100, -200), id = "baz" }

barNode = ApNode { title = "Bar", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
barPosNode = { node = barNode, pos = (100, 100), id = "bar" }

fooBarEdge = { from = (["foo"], ApResultSlot "out1"), to = (["bar"], ApParamSlot "InA") }
--fooBazEdge = { from = ("foo", "out2"), to = ("baz", "InC") }

subBazNode = ApNode { title = "SubBaz", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
subBazPosNode = { node = subBazNode, pos = (-50, -50), id = "baz1" }

subBarNode = ApNode { title = "SubBar", params = ["InA", "InB", "InC"], results = ["out1", "out2"] }
subBarPosNode = { node = subBarNode, pos = (50, 50), id = "bar1" }

subBarSubBazEdge = { from = (["lambda", "bar1"], ApResultSlot "out1"), to = (["lambda", "baz1"], ApParamSlot "InA") }

lambdaNode = LambdaNode <| D.fromList [ (subBarPosNode.id, subBarPosNode) , (subBazPosNode.id, subBazPosNode) ]
lambdaPosNode = { node = lambdaNode, pos = (300, -200), id = "lambda" }

ifNode = IfNode
ifPosNode = { id = "if1", node = ifNode, pos = (-200, 300) }

nodes = [fooPosNode, bazPosNode, barPosNode, ifPosNode, lambdaPosNode]
edges = [fooBarEdge]

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
