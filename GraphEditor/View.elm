module GraphEditor.View where

import Text as T
import Graphics.Collage as C
import Color
import List as L
import Dict as D
import Maybe as M

import Debug

import Diagrams.Core (..)
import Diagrams.Align (..)
import Diagrams.Pad (..)
import Diagrams.Geom (..)
import Diagrams.Bezier (..)
import Diagrams.Layout (..)
import Diagrams.FillStroke (..)
import Diagrams.Actions (..)
import Diagrams.Query (..)
import Diagrams.Debug (..)

import GraphEditor.Model (..)
import GraphEditor.Styles (..)
import GraphEditor.Actions (..)
import GraphEditor.Util (..)

-- common elements
xGlyph : Color.Color -> Maybe Color.Color -> Diagram Tag Action
xGlyph lineColor bgColor =
  let smallLine = vline 11 { defLine | color <- lineColor, width <- 2 }
      rotLeft = rotate (-pi/4) smallLine
      rotRight = rotate (pi/4) smallLine
      actualBgColor = M.withDefault (Color.red `withAlpha` 0) bgColor
      bg = circle 7 <| justFill <| C.Solid actualBgColor
  in zcat [rotLeft, rotRight, bg]

nodeXGlyph c = xGlyph c Nothing
edgeXGlyph bgC = xGlyph Color.black <| Just bgC

-- TODO: color code based on state
portCirc color = circle 7 (justFill <| C.Solid color)

inSlotLabel : InSlotId -> String
inSlotLabel sid =
    case sid of
      ApParamSlot name -> name
      IfCondSlot -> "condition"
      IfTrueSlot -> "if true"
      IfFalseSlot -> "if false"

inSlot : State -> InPortId -> LayoutRow Tag Action
inSlot state (nodePath, slotId) =
    let stateColor = portStateColorCode <| inPortState state (nodePath, slotId)
    in flexRight <| hcat [ tagWithActions (InPortT slotId) (inPortActions state (nodePath, slotId))
                              <| portCirc stateColor
                         , hspace 5
                         , text (inSlotLabel slotId) slotLabelStyle
                         ]

outSlotLabel : OutSlotId -> String
outSlotLabel sid =
    case sid of
      ApResultSlot name -> name
      IfResultSlot -> "result"
      FuncValueSlot -> "" -- not used

outSlot : State -> OutPortId -> LayoutRow Tag Action
outSlot state (nodePath, slotId) =
    let stateColor = portStateColorCode <| outPortState state (nodePath, slotId)
    in flexLeft <| hcat [ text (outSlotLabel slotId) slotLabelStyle
                        , hspace 5
                        , tagWithActions (OutPortT slotId) (outPortActions state (nodePath, slotId))
                            <| portCirc stateColor
                        ]

nodeTitle : String -> Color.Color -> NodePath -> Diagram Tag Action
nodeTitle name color nodePath =
    let title = text name titleStyle
        xOut = tagWithActions XOut (nodeXOutActions nodePath) <| nodeXGlyph color
    in hcat <| [ xOut
               , hspace 5
               , title
               , hspace 5
               ]

type SlotGroup = InputGroup (List InSlotId)
               | OutputGroup (List OutSlotId)

nodeDiagram : NodePath -> State -> LayoutRow Tag Action -> List SlotGroup -> Color.Color -> Diagram Tag Action
nodeDiagram nodePath state titleRow slotGroups color =
    let viewGroup : SlotGroup -> List (LayoutRow Tag Action)
        viewGroup group =
            case group of
              InputGroup ids -> L.map (\inSlotId -> inSlot state (nodePath, inSlotId)) ids
              OutputGroup ids -> L.map (\outSlotId -> outSlot state (nodePath, outSlotId)) ids
    in background (fillAndStroke (C.Solid color) defaultStroke) <|
          layout <| [titleRow, hrule nodeTopDivider 3]
                      ++ (intercalate [hrule nodeMiddleDivider 3] (L.map viewGroup slotGroups))

-- TODO: can cache diagram in PosNode to improve performance
viewPosNode : State -> NodePath -> PosNode -> Diagram Tag Action
viewPosNode state pathAbove pn =
  let nodePath = pathAbove ++ [pn.id]
  in pn.cachedDiagram
      |> tagWithActions (NodeIdT pn.id) (posNodeActions nodePath state.dragState)
      |> move pn.pos

viewNode : Node -> NodePath -> State -> Diagram Tag Action
viewNode node nodePath state = let a = Debug.log "viewNode" 2
                               in (alignTop <| alignLeft <|
                                      case node of
                                        ApNode attrs -> viewApNode attrs nodePath state
                                        IfNode -> viewIfNode nodePath state
                                        LambdaNode attrs -> viewLambdaNode attrs nodePath state)


-- BUG: flickers when mouse gets inside of its own canvas. need to think this through.
viewLambdaNode : LambdaNodeAttrs -> NodePath -> State -> Diagram Tag Action
viewLambdaNode node nodePath state =
    let -- TODO: this is same as viewApNode; factor out
        funcOutPortColor = portStateColorCode <| outPortState state (nodePath, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions state (nodePath, FuncValueSlot))
                          <| portCirc funcOutPortColor
        ln = Debug.log "lambda nodePath" nodePath
        titleRow = flexCenter (nodeTitle "Lambda" Color.black nodePath) funcOutPort
        nodes = zcat <| L.map (viewPosNode state nodePath) <| D.values node.nodes
        subCanvas = centered <| tagWithActions Canvas (canvasActions nodePath state.dragState) <|
                      zcat [nodes, rect node.dims.width node.dims.height invisible]
    in background (fillAndStroke (C.Solid lambdaNodeBgColor) defaultStroke) <|
          layout <| [titleRow, hrule nodeTopDivider 3, subCanvas]

-- TODO: padding is awkward
viewApNode : ApNodeAttrs -> NodePath -> State -> Diagram Tag Action
viewApNode node nodePath state =
    let funcOutPortColor = portStateColorCode <| outPortState state (nodePath, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions state (nodePath, FuncValueSlot))
                          <| portCirc funcOutPortColor
        titleRow = flexCenter (nodeTitle node.title Color.white nodePath) funcOutPort
        params = InputGroup <| L.map ApParamSlot node.params
        results = OutputGroup <| L.map ApResultSlot node.results
    in nodeDiagram nodePath state titleRow [params, results] apNodeBgColor -- TODO: lighter

viewIfNode : NodePath -> State -> Diagram Tag Action
viewIfNode nodePath state =
    let titleRow = flexRight (nodeTitle "If" Color.white nodePath)
        inSlots = InputGroup [IfCondSlot, IfTrueSlot, IfFalseSlot]
        outSlots = OutputGroup [IfResultSlot]
    in nodeDiagram nodePath state titleRow [inSlots, outSlots] ifNodeBgColor

--viewLambdaNode : ...

-- edges

viewEdge : Diagram Tag Action -> Edge -> Diagram Tag Action
viewEdge nodesDia edg =
   let {from, to} = getEdgeCoords nodesDia edg
   in viewGenericEdge from to

viewGenericEdge : Point -> Point -> Diagram Tag Action
viewGenericEdge fromCoords toCoords =
   let (fcx, fcy) = fromCoords
       (tcx, tcy) = toCoords
       cpSpacing = 100
   in bezier fromCoords (fcx+cpSpacing, fcy)
             (tcx-cpSpacing, tcy) toCoords
             edgeStyle
   --in path [fromCoords, toCoords] edgeStyle

viewDraggingEdge : OutPortId -> Diagram Tag Action -> Point -> Diagram Tag Action
viewDraggingEdge outPort nodesDia mousePos =
   viewGenericEdge (getOutPortCoords nodesDia outPort) mousePos

-- TODO: these are annoyingly similar
getOutPortCoords : Diagram Tag Action -> OutPortId -> Point
getOutPortCoords nodesDia outPort =
    let (nodePath, slotId) = outPort
        tagPath = (L.intersperse Canvas <| L.map NodeIdT nodePath)
    in case getCoords nodesDia (tagPath ++ [OutPortT slotId]) of
         Just pt -> pt
         Nothing -> Debug.crash ("path not found: " ++ (toString nodePath))

getInPortCoords : Diagram Tag Action -> InPortId -> Point
getInPortCoords nodesDia outPort =
   let (nodePath, slotId) = outPort
       tagPath = (L.intersperse Canvas <| L.map NodeIdT nodePath)
   in case getCoords nodesDia (tagPath ++ [InPortT slotId]) of
        Just pt -> pt
        Nothing -> Debug.crash ("path not found: " ++ (toString nodePath))

getEdgeCoords : Diagram Tag Action -> Edge -> { from : Point, to : Point }
getEdgeCoords nodesDia edg =
  { from = getOutPortCoords nodesDia edg.from
  , to = getInPortCoords nodesDia edg.to
  }

viewEdgeXOut : Diagram Tag Action -> Edge -> Diagram Tag Action
viewEdgeXOut nodesDia edge =
  let edgeCoords = getEdgeCoords nodesDia edge
  in tagWithActions XOut (edgeXOutActions edge) <| move edgeCoords.to <| edgeXGlyph normalPortColor

viewGraph : State -> Diagram Tag Action
viewGraph state = 
    let nodes = zcat <| L.map (viewPosNode state []) <| D.values state.graph.nodes
        edges = zcat <| L.map (viewEdge nodes) state.graph.edges
        edgeXOuts = zcat <| L.map (viewEdgeXOut nodes) state.graph.edges
        draggingEdge = case state.dragState of
                         Just (DraggingEdge attrs) -> [viewDraggingEdge attrs.fromPort nodes attrs.endPos]
                         _ -> []
    in tagWithActions Canvas (canvasActions [] state.dragState) <| pad 10000 <| zcat <| draggingEdge ++ [edgeXOuts, edges, nodes]
-- TODO: pad 10000 is jank
