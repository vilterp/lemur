module Diagrams.Interact where

{-| Abstractions for making diagrams which change as a function of the mouse.

# Function Types
@docs RenderFunc, UpdateFunc, InteractUpdateFunc

# Interaction
@docs InteractionState, initInteractionState, interactFold, makeFoldUpdate

# Mouse Event Processing
@docs processMouseEvent

# Mouse State
@docs MouseState, initMouseState
-}

import Signal as S
import Window
import Mouse

import List as L
import Maybe as M
import Graphics.Element as E
import Graphics.Collage as C

import Diagrams.Core (..)
import Diagrams.Query (..)
import Diagrams.Geom (..)
import Diagrams.Actions (..)
import Diagrams.Wiring (..)

import Debug

-- BUG: if A is on top of and within B, entering A should not count as leaving B.
-- shit, I guess the pick path should really be a pick tree. #@$@

-- TODO(perf): keep both unzipped for perforance?
type alias MouseState t a =
    { isDown : Bool
    , overPath : PickPath t a
    , overTags : List t
    , pickPathOnMouseDown : Maybe (List t)
    }

initMouseState = { isDown = False, overPath = [], overTags = [], pickPathOnMouseDown = Nothing }

type alias InteractionState m t a =
    { mouseState : MouseState t a
    , diagram : Diagram t a
    , modelState : m
    }

type alias RenderFunc m t a = m -> Diagram t a
type alias UpdateFunc m a = a -> m -> m
type alias InteractUpdateFunc m t a = (CollageLocation, PrimMouseEvent) -> InteractionState m t a -> InteractionState m t a

{-| Top-level interface to this module. Given
- how to update the state (type `m`) given an action (type `a`),
- how to render a diagram given the state,
- and how to compute the location of the collage on screen from the window dimensions,
Return a signal of diagrams.
-}
interactFold : UpdateFunc m a -> RenderFunc m t a -> CollageLocFunc -> m -> Signal (Diagram t a)
interactFold updateF renderF collageLocF initModel =
    let states = S.foldp (makeFoldUpdate updateF renderF)
                         (initInteractState renderF initModel)
                         (makeUpdateStream collageLocF)
    in S.map .diagram states

makeFoldUpdate : UpdateFunc m a -> RenderFunc m t a -> InteractUpdateFunc m t a
makeFoldUpdate updateF renderF =
    \(loc, evt) intState ->
        let (newMS, actions) = processMouseEvent intState.diagram intState.mouseState evt
            --actions = Debug.watch "actions" actions
            -- new model
            oldModel = intState.modelState
            newModel = L.foldr updateF oldModel actions
            -- re-render
            oldDiagram = intState.diagram
            newDiagram = if oldModel == newModel
                         then oldDiagram
                         else renderF newModel
        in { mouseState = newMS
           , diagram = newDiagram
           , modelState = Debug.watch "state" newModel
           }

initInteractState : RenderFunc m t a -> m -> InteractionState m t a
initInteractState render model =
    { mouseState = initMouseState
    , modelState = model
    , diagram = render model
    }

-- BUG: no initial pick path

-- TODO: fix these docs vv
{-| Given diagram with mouse state (`MouseDiagram`), mouse event, and dimensions of collage, return
new `MouseDiagram` with list of actions triggered by this mouse event. -}
processMouseEvent : Diagram t a -> MouseState t a -> PrimMouseEvent -> (MouseState t a, List a)
processMouseEvent diagram mouseState (evt, mousePos) =
    let applyActions overPath = mapWithEarlyStop (\(pt, e2a) -> e2a <| MouseEvent { pickPath = overPath, offset = pt })
        overPath = pick diagram mousePos -- need to pick every time because actions may have changed
    in case evt of
         MouseDownEvt -> let actions = L.filterMap (getOffsetAndMember .mouseDown) overPath
                         in ( { mouseState | isDown <- True
                                           , pickPathOnMouseDown <- Just <| L.map .tag overPath }
                            , applyActions overPath actions
                            )
         MouseUpEvt -> let overTags = L.map .tag overPath
                           mouseUps = L.filterMap (getOffsetAndMember .mouseUp) overPath
                           --a = Debug.log "---------------" ()
                           --b = Debug.log "op:" overPath
                           --c = Debug.log "ppomd:" mouseState.pickPathOnMouseDown
                           --d = Debug.log "match" (b == (M.withDefault [] c))
                           -- TODO: filter for ones that have same pick path on mouse down as now (?)
                           clicks = if L.map .tag overPath == M.withDefault [] mouseState.pickPathOnMouseDown
                                    then L.filterMap (getOffsetAndMember .click) overPath
                                    else []
                       in ( { mouseState | isDown <- False
                                         , pickPathOnMouseDown <- Nothing }
                          , applyActions overPath <| clicks ++ mouseUps
                          )
         MouseMoveEvt -> let oldOverPath = mouseState.overPath
                             -- sets of tags of elements mouse has left or entered
                             overTags = L.map .tag overPath
                             oldOverTags = mouseState.overTags
                             -- action sets corresponding to these tags
                             enters = L.filterMap (getOffsetAndMember .mouseEnter) <|
                                         L.filter (\ppe -> not <| L.member ppe.tag oldOverTags) overPath
                             leaves = L.filterMap (getOffsetAndMember .mouseLeave) <|
                                         L.filter (\ppe -> not <| L.member ppe.tag overTags) oldOverPath
                             moves = L.filterMap (getOffsetAndMember .mouseMove) <|
                                         L.filter (\ppe -> L.member ppe.tag oldOverTags) overPath
                         in ( { mouseState | overPath <- overPath
                                           , overTags <- overTags }
                            , applyActions overPath <| enters ++ leaves ++ moves
                            )

-- helpers for processMouseEvent

getOffsetAndMember : (ActionSet t a -> Maybe (EventToAction t a)) -> PickPathElem t a -> Maybe (Point, EventToAction t a)
getOffsetAndMember getMember ppe = case getMember ppe.actionSet of
                                     Just e2a -> Just (ppe.offset, e2a)
                                     Nothing -> Nothing

{-| Like map, but stops if the second element of the function result is True. -}
mapWithEarlyStop : (a -> (b, Bool)) -> List a -> List b
mapWithEarlyStop f l =
    case l of
      [] -> []
      (x::xs) -> case f x of
                   (y, True) -> [y]
                   (y, False) -> y :: (mapWithEarlyStop f xs)
