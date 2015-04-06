module Diagrams.Bezier where

{-| Generate bezier paths as lists of points.
Adapted from [this gist](https://gist.github.com/irrwitz/968b9762819974c92c9f).

@docs bezier
-}

import Graphics.Collage as C
import List as L

import Diagrams.Core (..)
import Diagrams.Geom (..)

-- TODO: replace this entire module/approach with builtin arcs...

{-| Given four points a, cp1, cp2, b, return path diagram which is a bezier
curve from a to b, using cp1 and cp2 as control points. -}
bezier : Point -> Point -> Point -> Point -> C.LineStyle -> Diagram t a
bezier a cp1 cp2 b ls = path (bezierCurve a cp1 cp2 b) ls

bezierCurve : Point -> Point -> Point -> Point -> List Point
bezierCurve a cp1 cp2 b = L.map (\x -> bezierPoint x [a, cp1, cp2, b]) resolution

bezierPoint : Float -> List Point -> Point
bezierPoint t points = if | (L.length points == 1) -> L.head points 
                          | otherwise -> bezierPoint t (L.map2 (interpolatePoint t) points (L.tail points))

interpolatePoint : Float -> Point -> Point -> Point
interpolatePoint t (x0, y0) (x1, y1) = (lerp (x0, x1) (0, 1) t, lerp (y0, y1) (0, 1) t)

{--
  An array [0, 0.01, 0.02, ..., 1] which defines the resolution of the curve
--}
resolution : List Float
resolution = generate 0 1.0 0.01

{--
  generate: Creates a array of floats beginning from the given start value 
  until end value. Values in between are start value plus multiples
  of step value until end is reached or exceeded.
--}
generate : Float -> Float -> Float -> List Float
generate start end step = if | end <= start -> []
                             | start + step >= end -> [start, end]
                             | otherwise -> [start] ++ generate (start + step) end step
