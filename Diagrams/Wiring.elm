module Diagrams.Wiring where

{-| Functions and types for getting a (possibly interactive) diagram onto the screen.

# Types
@docs CollageLocation, PrimMouseEvent, PrimMouseEvtType, CollageLocFunc

# Functions
@docs makeUpdateStream, mouseEvents, offsetMousePos

# Input Signals
@docs floatMousePos, floatWindowDims
-}

import Signal as S
import Mouse
import Window
import Graphics.Collage as C

import Diagrams.Geom (..)

{-| Where offset is difference between top left of screen and top left of collage,
increasing right and down. -}
type alias CollageLocation = OffsetDimsBox

type alias PrimMouseEvent = (PrimMouseEvtType, Point)

type PrimMouseEvtType = MouseUpEvt
                      | MouseDownEvt
                      | MouseMoveEvt

{-| Given window size, where on screen and how big is your collage? -}
type alias CollageLocFunc = Dims -> CollageLocation

-- TODO: clip events that aren't within the collage loc

{-| Given collage location function, return stream of (collage location, mouse event)
pairs, where mouse coordinates are relative to the center of the collage at its present
location, and increasing up and to the right. -}
makeUpdateStream : CollageLocFunc -> Signal (CollageLocation, PrimMouseEvent)
makeUpdateStream clf =
    let collageLocs = S.map clf floatWindowDims
        mouseEvts = mouseEvents collageLocs
    in S.map2 (,) collageLocs mouseEvts

mouseEvents : Signal CollageLocation -> Signal PrimMouseEvent
mouseEvents loc =
    let upDown = S.map (\down -> if down then MouseDownEvt else MouseUpEvt) Mouse.isDown
        moves = S.map (always MouseMoveEvt) Mouse.position
        events = S.merge upDown moves
        adjustedMousePos = S.map2 offsetMousePos loc floatMousePos
    in S.map2 (,) events adjustedMousePos

{-| Given the position of the top-left of a collage (from the top-left of the screen; coords increasing right and down)
and the dimensions of the collage, return a signal of the mouse position relative to the center of that collage,
and increasing up and to the right instead of down and to the right. -}
offsetMousePos : CollageLocation -> Point -> Point
offsetMousePos loc (x, y) = let (offsetX, offsetY) = loc.offset
                                {width, height} = loc.dims
                            in (x - width/2 - offsetX, (height/2 + offsetY) - y)

-- input signals

toPoint : (Int, Int) -> Point
toPoint (x, y) = (toFloat x, toFloat y)

floatMousePos : Signal Point
floatMousePos = S.map toPoint Mouse.position

floatWindowDims : Signal Dims
floatWindowDims = S.map (\(w, h) -> { width = toFloat w, height = toFloat h })
                        Window.dimensions
