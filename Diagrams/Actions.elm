module Diagrams.Actions where

{-| A type for attaching mouse actions to diagrams.

@docs ActionSet, EventToAction, emptyActionSet, collageMousePos
-}

import List as L
import Debug

import Diagrams.Geom (..)

-- TODO: tagging function easier than `tagWithAction tag { emptyActionSet | click <- Just ... } dia`?
-- like `clickable tag func dia` or something
-- or list of attributes like html

-- TODO: odd place for these to live
{-| (Tag, Coordinates) pairs from bottom of tree to top; result of
calling `pick` (see below). -}
type alias PickPath t a = List (PickPathElem t a)
type alias PickPathElem t a = { tag : t
                              , actionSet : ActionSet t a
                              , offset : Point
                              }
type MouseEvent t a = MouseEvent { offset : Point
                                 , pickPath : PickPath t a
                                 }

{-| Given an event, return (a) an action resulting from that event, and (b) whether to stop this
mouse event from "bubbling up" to handlers higher up the tree. -}
type alias EventToAction t a = MouseEvent t a -> (a, Bool)
type alias ActionSet t a =
    { click : Maybe (EventToAction t a)
    , mouseEnter : Maybe (EventToAction t a)
    , mouseLeave : Maybe (EventToAction t a)
    , mouseMove : Maybe (EventToAction t a)
    , mouseDown : Maybe (EventToAction t a)
    , mouseUp : Maybe (EventToAction t a)
    }

emptyActionSet =
    { click = Nothing
    , mouseEnter = Nothing
    , mouseLeave = Nothing
    , mouseMove = Nothing
    , mouseDown = Nothing
    , mouseUp = Nothing
    }

keepBubbling : (MouseEvent t a -> a) -> EventToAction t a
keepBubbling f = \evt -> (f evt, False)

stopBubbling : (MouseEvent t a -> a) -> EventToAction t a
stopBubbling f = \evt -> (f evt, True)

-- why is this not in the stdlib?
-- empty list => incomplete pattern match
last : List a -> a
last l = case l of
           [] -> Debug.crash "last expects a non-empty list"
           [x] -> x
           (x::xs) -> last xs

collageMousePos : MouseEvent t a -> Point
collageMousePos (MouseEvent evt) = (last evt.pickPath).offset
