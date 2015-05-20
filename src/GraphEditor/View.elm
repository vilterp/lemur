module GraphEditor.View where

import Text as T
import Graphics.Collage as C
import Color
import List as L
import Dict as D
import Maybe as M

import Debug

import Diagrams.Core exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Bezier exposing (..)
import Diagrams.Layout exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Debug exposing (..)

import Model exposing (..)
import GraphEditor.Model exposing (..)
import GraphEditor.Styles exposing (..)
import GraphEditor.Actions exposing (..)
import Util exposing (..)

type alias GEDiagram = Diagram Tag GraphEditorAction

-- common elements
xGlyph : Color.Color -> Maybe Color.Color -> GEDiagram
xGlyph lineColor bgColor =
  let smallLine = vline 11 { defLine | color <- lineColor, width <- 2 }
      rotLeft = rotate (-pi/4) smallLine
      rotRight = rotate (pi/4) smallLine
      -- TODO: get with alpha to work (?)
      actualBgColor = M.withDefault (Color.red |> withAlpha 0) bgColor
      bg = circle 7 <| justFill <| Solid actualBgColor
  in zcat [rotLeft, rotRight, bg]

nodeXGlyph c = xGlyph c Nothing
edgeXGlyph bgC = xGlyph Color.black <| Just bgC

portCirc color = circle 7 (justFill <| Solid color)

inSlotLabel : InSlotId -> String
inSlotLabel sid =
    case sid of
      ApParamSlot name -> name
      IfCondSlot -> "condition"
      IfTrueSlot -> "if true"
      IfFalseSlot -> "if false"

inSlot : GraphViewModel -> InPortId -> LayoutRow Tag GraphEditorAction
inSlot viewModel (nodePath, slotId) =
    let stateColor = portStateColorCode <| inPortState viewModel (nodePath, slotId)
    in [ portCirc stateColor
          |> tagWithActions (InPortT slotId) (inPortActions viewModel (nodePath, slotId))
       , hspace 5
       , inSlotLabel slotId |> text slotLabelStyle
       ]
        |> hcat
        |> flexRight

outSlotLabel : OutSlotId -> String
outSlotLabel sid =
    case sid of
      ApResultSlot name -> name
      IfResultSlot -> "result"
      FuncValueSlot -> "" -- not used

outSlot : GraphViewModel -> OutPortId -> LayoutRow Tag GraphEditorAction
outSlot viewModel (nodePath, slotId) =
    let stateColor = portStateColorCode <| outPortState viewModel (nodePath, slotId)
    in [ outSlotLabel slotId |> text slotLabelStyle
       , hspace 5
       , portCirc stateColor
          |> tagWithActions (OutPortT slotId) (outPortActions viewModel (nodePath, slotId))
       ]
        |> hcat
        |> flexLeft

nodeTitle : String -> Color.Color -> NodePath -> GEDiagram
nodeTitle name color nodePath =
    let title = name |> text titleStyle
        xOut = tagWithActions XOut (nodeXOutActions nodePath) <| nodeXGlyph color
    in hcat <| [ xOut
               , hspace 5
               , title
               , hspace 5
               ]

type SlotGroup = InputGroup (List InSlotId)
               | OutputGroup (List OutSlotId)

nodeDiagram : NodePath -> GraphViewModel -> LayoutRow Tag GraphEditorAction -> List SlotGroup -> Color.Color -> GEDiagram
nodeDiagram nodePath viewModel titleRow slotGroups color =
    let viewGroup : SlotGroup -> List (LayoutRow Tag GraphEditorAction)
        viewGroup group =
            case group of
              InputGroup ids -> L.map (\inSlotId -> inSlot viewModel (nodePath, inSlotId)) ids
              OutputGroup ids -> L.map (\outSlotId -> outSlot viewModel (nodePath, outSlotId)) ids
    in [titleRow, hrule nodeTopDivider 3]
        ++ (intercalate [hrule nodeMiddleDivider 3] (L.map viewGroup slotGroups))
        |> layout
        |> background (fillAndStroke (Solid color) defaultStroke)
                      

-- TODO: can cache diagram in PosNode to improve performance
viewPosNode : GraphViewModel -> NodePath -> PosNode -> GEDiagram
viewPosNode viewModel pathAbove pn =
  let nodePath = pathAbove ++ [pn.id]
  in viewNode pn.node nodePath viewModel
      |> tagWithActions (NodeIdT pn.id) (posNodeActions nodePath viewModel.editorState.mouseInteractionState)
      |> move pn.pos

viewNode : Node -> NodePath -> GraphViewModel -> GEDiagram
viewNode node nodePath viewModel =
    alignTop <| alignLeft <|
      case node of
        ApNode attrs -> viewApNode attrs nodePath viewModel
        IfNode -> viewIfNode nodePath viewModel
        LambdaNode attrs -> viewLambdaNode attrs nodePath viewModel

-- BUG: flickers when mouse gets inside of its own canvas. need to think this through.
viewLambdaNode : LambdaNodeAttrs -> NodePath -> GraphViewModel -> GEDiagram
viewLambdaNode node nodePath viewModel =
    let -- TODO: this is same as viewApNode; factor out
        funcOutPortColor = portStateColorCode <| outPortState viewModel (nodePath, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions viewModel (nodePath, FuncValueSlot))
                          <| portCirc funcOutPortColor
        titleRow = flexCenter (nodeTitle "Lambda" Color.black nodePath) funcOutPort
        nodes = zcat <| L.map (viewPosNode viewModel nodePath) <| D.values node.nodes
        subCanvas =
            [nodes, rect node.dims.width node.dims.height invisible]
              |> zcat
              |> pad 7
              |> tagWithActions Canvas (canvasActions nodePath viewModel.editorState.mouseInteractionState)
              |> centered
        lState = lambdaState viewModel nodePath
    in [titleRow, hrule nodeTopDivider 3, subCanvas]
          |> layout
          |> background (fillAndStroke (Solid <| lambdaNodeBgColor lState) defaultStroke)

-- TODO: padding is awkward
viewApNode : FuncId -> NodePath -> GraphViewModel -> GEDiagram
viewApNode funcId nodePath viewModel =
    let func = getFunc viewModel.mod funcId |> getMaybeOrCrash "no such func"
        funcOutPortColor = portStateColorCode <| outPortState viewModel (nodePath, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions viewModel (nodePath, FuncValueSlot))
                          <| portCirc funcOutPortColor
        titleRow = flexCenter (nodeTitle (func |> funcName) Color.white nodePath) funcOutPort
        params = InputGroup <| L.map ApParamSlot (func |> funcParams viewModel.mod)
        results = OutputGroup <| L.map ApResultSlot (func |> funcReturnVals viewModel.mod)
    in nodeDiagram nodePath viewModel titleRow [params, results] apNodeBgColor -- TODO: lighter

viewIfNode : NodePath -> GraphViewModel -> GEDiagram
viewIfNode nodePath viewModel =
    let titleRow = flexRight (nodeTitle "If" Color.white nodePath)
        inSlots = InputGroup [IfCondSlot, IfTrueSlot, IfFalseSlot]
        outSlots = OutputGroup [IfResultSlot]
    in nodeDiagram nodePath viewModel titleRow [inSlots, outSlots] ifNodeBgColor

-- edges

viewEdge : GEDiagram -> Edge -> GEDiagram
viewEdge nodesDia edg =
   let {from, to} = getEdgeCoords nodesDia edg
   in viewGenericEdge from to

viewGenericEdge : Point -> Point -> GEDiagram
viewGenericEdge fromCoords toCoords =
   let (fcx, fcy) = fromCoords
       (tcx, tcy) = toCoords
       cpSpacing = 100
   in bezier fromCoords (fcx+cpSpacing, fcy)
             (tcx-cpSpacing, tcy) toCoords
             edgeStyle

viewDraggingEdge : OutPortId -> GEDiagram -> Point -> GEDiagram
viewDraggingEdge outPort nodesDia mousePos =
   viewGenericEdge (getOutPortCoords nodesDia outPort) mousePos

-- TODO: these are annoyingly similar
getOutPortCoords : GEDiagram -> OutPortId -> Point
getOutPortCoords nodesDia outPort =
    let (nodePath, slotId) = outPort
        tagPath = (L.intersperse Canvas <| L.map NodeIdT nodePath)
    in case getCoords nodesDia (tagPath ++ [OutPortT slotId]) of
         Just pt -> pt
         Nothing -> Debug.crash ("path not found: " ++ (toString nodePath))

getInPortCoords : GEDiagram -> InPortId -> Point
getInPortCoords nodesDia outPort =
   let (nodePath, slotId) = outPort
       tagPath = (L.intersperse Canvas <| L.map NodeIdT nodePath)
   in case getCoords nodesDia (tagPath ++ [InPortT slotId]) of
        Just pt -> pt
        Nothing -> Debug.crash ("path not found: " ++ (toString nodePath))

getEdgeCoords : GEDiagram -> Edge -> { from : Point, to : Point }
getEdgeCoords nodesDia edg =
    { from = getOutPortCoords nodesDia edg.from
    , to = getInPortCoords nodesDia edg.to
    }

viewEdgeXOut : GEDiagram -> Edge -> GEDiagram
viewEdgeXOut nodesDia edge =
  let edgeCoords = getEdgeCoords nodesDia edge
  in edgeXGlyph normalPortColor
      |> move edgeCoords.to
      |> tagWithActions XOut (edgeXOutActions edge)

viewGraph : GraphViewModel -> GEDiagram
viewGraph viewModel = 
    -- TODO: draw lambda nodes under other nodes
    let graph = viewModel.currentGraph
        --g = Debug.log "graph" graph.nodes
        nodes = zcat <| L.map (viewPosNode viewModel []) <| D.values graph.nodes
        edges = zcat <| L.map (viewEdge nodes) graph.edges
        edgeXOuts = zcat <| L.map (viewEdgeXOut nodes) graph.edges
        draggingEdge =
            case viewModel.editorState.mouseInteractionState of
              Just (Dragging (DraggingEdge attrs)) ->
                  [viewDraggingEdge attrs.fromPort nodes attrs.endPos]
              _ -> []
        -- TODO: tooltip
    in draggingEdge ++ [edgeXOuts, edges, nodes]
        |> zcat
        |> pad 10000
        |> tagWithActions Canvas (canvasActions [] viewModel.editorState.mouseInteractionState)
        |> move viewModel.editorState.pan
        |> tagWithActions TopLevel (topLevelActions viewModel.editorState.mouseInteractionState)
-- TODO: pad 10000 is jank
