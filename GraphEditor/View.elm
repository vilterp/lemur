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

-- Styles (TODO: factor into own module?)

defaultTextStyle = T.defaultStyle
titleStyle = { defaultTextStyle | bold <- True }
slotLabelStyle = defaultTextStyle

defaultLineStyle = C.defaultLine

edgeStyle = { defaultLineStyle | width <- 3 }

defLine = C.defaultLine

nodeTopDivider = defLine
nodeMiddleDivider = { defLine | dashing <- [5, 5] }

portColor = Color.yellow

-- actions

posNodeActions nodePath dragState =
    case dragState of
      Nothing -> { emptyActionSet | mouseDown <- Just <| stopBubbling <|
                                      \(MouseEvent evt) -> DragNodeStart { nodePath = nodePath, offset = evt.offset } }
      _ -> emptyActionSet

nodeXOutActions nodePath = { emptyActionSet | click <- Just <| keepBubbling <| always <| RemoveNode nodePath }

edgeXOutActions edge = { emptyActionSet | click <- Just <| stopBubbling <| always <| RemoveEdge edge }

canvasActions dragState =
    let dragMove = { emptyActionSet | mouseMove <- Just <| stopBubbling <| \(MouseEvent evt) -> DragMove evt.offset
                                    , mouseUp <- Just <| stopBubbling <| always DragEnd }
    in case dragState of
         Nothing -> emptyActionSet
         _ -> dragMove

outPortActions : OutPortId -> ActionSet Tag Action
outPortActions portId = { emptyActionSet | mouseDown <- Just <| stopBubbling <|
                                              \evt -> DragEdgeStart { fromPort = portId, endPos = collageMousePos evt } }

inPortActions : InPortId -> State -> ActionSet Tag Action
inPortActions portId state =
    let portState = inPortState state portId
    in case state.dragState of
         Just (DraggingEdge attrs) -> if portState == ValidPort
                                      then { emptyActionSet | mouseUp <- Just <| stopBubbling
                                                <| always <| AddEdge { from = attrs.fromPort, to = portId } }
                                      else emptyActionSet
         _ -> emptyActionSet

-- TODO: don't forget about ports that are taken
portStateColorCode : PortState -> Color.Color
portStateColorCode st = case st of
                          ValidPort -> Color.lightGreen
                          InvalidPort -> Color.yellow
                          NormalPort -> Color.yellow
                          TakenPort -> takenColor

takenColor = Color.lightGreen

-- views

-- common elements
xGlyph : Color.Color -> Maybe Color.Color -> Diagram Tag Action
xGlyph lineColor bgColor =
  let smallLine = vline 11 { defLine | color <- lineColor, width <- 2 }
      rotLeft = rotate (-pi/4) smallLine
      rotRight = rotate (pi/4) smallLine
      actualBgColor = M.withDefault (Color.red `withAlpha` 0) bgColor
      bg = circle 7 <| justFill <| C.Solid actualBgColor
  in zcat [rotLeft, rotRight, bg]

nodeXGlyph = xGlyph Color.white Nothing
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
    in flexRight <| hcat [ tagWithActions (InPortT slotId) (inPortActions (nodePath, slotId) state)
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
                        , tagWithActions (OutPortT slotId) (outPortActions (nodePath, slotId))
                            <| portCirc stateColor
                        ]

nodeTitle : String -> NodePath -> Diagram Tag Action
nodeTitle name nodePath =
    let title = text name titleStyle
        xOut = tagWithActions XOut (nodeXOutActions nodePath) <| nodeXGlyph
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
  in viewNode pn.node nodePath state
      |> tagWithActions (NodeIdT pn.id) (posNodeActions nodePath state.dragState)
      |> move pn.pos

viewNode : Node -> NodePath -> State -> Diagram Tag Action
viewNode node nodePath state =
    case node of
      ApNode attrs -> viewApNode attrs nodePath state
      IfNode -> viewIfNode nodePath state
      LambdaNode _ -> empty

-- TODO: padding is awkward
viewApNode : ApNodeAttrs -> NodePath -> State -> Diagram Tag Action
viewApNode node nodePath state =
    let funcOutPortColor = portStateColorCode <| outPortState state (nodePath, FuncValueSlot)
        funcOutPort = tagWithActions (OutPortT FuncValueSlot) (outPortActions (nodePath, FuncValueSlot))
                          <| portCirc funcOutPortColor
        titleRow = flexCenter (nodeTitle node.title nodePath) funcOutPort
        params = InputGroup <| L.map ApParamSlot node.params
        results = OutputGroup <| L.map ApResultSlot node.results
    in nodeDiagram nodePath state titleRow [params, results] Color.lightBlue -- TODO: lighter

viewIfNode : NodePath -> State -> Diagram Tag Action
viewIfNode nodePath state =
    let titleRow = flexRight (nodeTitle "If" nodePath)
        inSlots = InputGroup [IfCondSlot, IfTrueSlot, IfFalseSlot]
        outSlots = OutputGroup [IfResultSlot]
    in nodeDiagram nodePath state titleRow [inSlots, outSlots] Color.lightPurple

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

viewDraggingEdge : OutPortId -> Diagram Tag Action -> Point -> Diagram Tag Action
viewDraggingEdge outPort nodesDia mousePos =
   viewGenericEdge (getOutPortCoords nodesDia outPort) mousePos

-- TODO: these are annoyingly similar
getOutPortCoords : Diagram Tag Action -> OutPortId -> Point
getOutPortCoords nodesDia outPort =
    let (nodePath, slotId) = outPort
        tagPath = L.map NodeIdT nodePath
    in case getCoords nodesDia (tagPath ++ [OutPortT slotId]) of
         Just pt -> pt
         Nothing -> Debug.crash ("path not found: " ++ (toString nodePath))

getInPortCoords : Diagram Tag Action -> InPortId -> Point
getInPortCoords nodesDia outPort =
   let (nodePath, slotId) = outPort
       tagPath = L.map NodeIdT nodePath
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
  in tagWithActions XOut (edgeXOutActions edge) <| move edgeCoords.to <| edgeXGlyph takenColor

viewGraph : State -> Diagram Tag Action
viewGraph state = 
    let nodes = zcat <| L.map (viewPosNode state []) <| D.values state.graph.nodes
        edges = zcat <| L.map (viewEdge nodes) state.graph.edges
        edgeXOuts = zcat <| L.map (viewEdgeXOut nodes) state.graph.edges
        draggingEdge = case state.dragState of
                         Just (DraggingEdge attrs) -> [viewDraggingEdge attrs.fromPort nodes attrs.endPos]
                         _ -> []
    in tagWithActions Canvas (canvasActions state.dragState) <| pad 10000 <| zcat <| draggingEdge ++ [edgeXOuts, edges, nodes]
-- TODO: pad 10000 is jank

-- util

intercalate : List a -> List (List a) -> List a
intercalate sep xs = L.concat <| L.intersperse sep xs
