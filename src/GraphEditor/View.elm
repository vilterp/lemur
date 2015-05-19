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

inSlot : State -> InPortId -> LayoutRow Tag GraphEditorAction
inSlot state (nodePath, slotId) =
    let stateColor = portStateColorCode <| inPortState state (nodePath, slotId)
    in flexRight <| hcat [ tagWithActions (InPortT slotId) (inPortActions state (nodePath, slotId))
                              <| portCirc stateColor
                         , hspace 5
                         , inSlotLabel slotId |> text slotLabelStyle
                         ]

outSlotLabel : OutSlotId -> String
outSlotLabel sid =
    case sid of
      ApResultSlot name -> name
      IfResultSlot -> "result"
      FuncValueSlot -> "" -- not used

outSlot : State -> OutPortId -> LayoutRow Tag GraphEditorAction
outSlot state (nodePath, slotId) =
    let stateColor = portStateColorCode <| outPortState state (nodePath, slotId)
    in flexLeft <| hcat [ outSlotLabel slotId |> text slotLabelStyle
                        , hspace 5
                        , tagWithActions (OutPortT slotId) (outPortActions state (nodePath, slotId))
                            <| portCirc stateColor
                        ]

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

nodeDiagram : NodePath -> State -> LayoutRow Tag GraphEditorAction -> List SlotGroup -> Color.Color -> GEDiagram
nodeDiagram nodePath state titleRow slotGroups color =
    let viewGroup : SlotGroup -> List (LayoutRow Tag GraphEditorAction)
        viewGroup group =
            case group of
              InputGroup ids -> L.map (\inSlotId -> inSlot state (nodePath, inSlotId)) ids
              OutputGroup ids -> L.map (\outSlotId -> outSlot state (nodePath, outSlotId)) ids
    in background (fillAndStroke (Solid color) defaultStroke) <|
          layout <| [titleRow, hrule nodeTopDivider 3]
                      ++ (intercalate [hrule nodeMiddleDivider 3] (L.map viewGroup slotGroups))

-- TODO: can cache diagram in PosNode to improve performance
viewPosNode : State -> NodePath -> PosNode -> GEDiagram
viewPosNode state pathAbove pn =
  let nodePath = pathAbove ++ [pn.id]
  in viewNode pn.node nodePath state
      |> tagWithActions (NodeIdT pn.id) (posNodeActions nodePath state.graphEditorState.dragState)
      |> move pn.pos

viewNode : Node -> NodePath -> State -> GEDiagram
viewNode node nodePath state =
    alignTop <| alignLeft <|
      case node of
        ApNode attrs -> viewApNode attrs nodePath state
        IfNode -> viewIfNode nodePath state
        LambdaNode attrs -> viewLambdaNode attrs nodePath state

-- BUG: flickers when mouse gets inside of its own canvas. need to think this through.
viewLambdaNode : LambdaNodeAttrs -> NodePath -> State -> GEDiagram
viewLambdaNode node nodePath state =
    let -- TODO: this is same as viewApNode; factor out
        funcOutPortColor = portStateColorCode <| outPortState state (nodePath, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions state (nodePath, FuncValueSlot))
                          <| portCirc funcOutPortColor
        titleRow = flexCenter (nodeTitle "Lambda" Color.black nodePath) funcOutPort
        nodes = zcat <| L.map (viewPosNode state nodePath) <| D.values node.nodes
        subCanvas = centered <| tagWithActions Canvas (canvasActions nodePath state.graphEditorState.dragState) <|
                      pad 7 <| zcat [nodes, rect node.dims.width node.dims.height invisible]
        lState = lambdaState state nodePath
    in background (fillAndStroke (Solid <| lambdaNodeBgColor lState) defaultStroke) <|
        layout <| [titleRow, hrule nodeTopDivider 3, subCanvas]

-- TODO: padding is awkward
viewApNode : FuncId -> NodePath -> State -> GEDiagram
viewApNode funcId nodePath state =
    let func = getFunc state.mod funcId |> getMaybeOrCrash "no such func"
        funcOutPortColor = portStateColorCode <| outPortState state (nodePath, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions state (nodePath, FuncValueSlot))
                          <| portCirc funcOutPortColor
        titleRow = flexCenter (nodeTitle (func |> funcName) Color.white nodePath) funcOutPort
        params = InputGroup <| L.map ApParamSlot (func |> funcParams state.mod)
        results = OutputGroup <| L.map ApResultSlot (func |> funcReturnVals state.mod)
    in nodeDiagram nodePath state titleRow [params, results] apNodeBgColor -- TODO: lighter

viewIfNode : NodePath -> State -> GEDiagram
viewIfNode nodePath state =
    let titleRow = flexRight (nodeTitle "If" Color.white nodePath)
        inSlots = InputGroup [IfCondSlot, IfTrueSlot, IfFalseSlot]
        outSlots = OutputGroup [IfResultSlot]
    in nodeDiagram nodePath state titleRow [inSlots, outSlots] ifNodeBgColor

--viewLambdaNode : ...

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
  in tagWithActions XOut (edgeXOutActions edge) <| move edgeCoords.to <| edgeXGlyph normalPortColor

viewGraph : State -> GEDiagram
viewGraph state = 
    -- TODO: draw lambda nodes under other nodes
    let graph = state |> getCurrentGraph
        --g = Debug.log "graph" graph.nodes
        nodes = zcat <| L.map (viewPosNode state []) <| D.values graph.nodes
        edges = zcat <| L.map (viewEdge nodes) graph.edges
        edgeXOuts = zcat <| L.map (viewEdgeXOut nodes) graph.edges
        draggingEdge = case state.graphEditorState.dragState of
                         Just (DraggingEdge attrs) -> [viewDraggingEdge attrs.fromPort nodes attrs.endPos]
                         _ -> []
        canvas = tagWithActions Canvas (canvasActions [] state.graphEditorState.dragState) <|
                    pad 10000 <| zcat <| draggingEdge ++ [edgeXOuts, edges, nodes]
    in tagWithActions TopLevel (topLevelActions state) <| move state.graphEditorState.pan canvas
-- TODO: pad 10000 is jank
