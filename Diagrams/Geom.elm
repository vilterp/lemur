module Diagrams.Geom where

{-|
# Geometry Utilities
@docs Transform, Point, applyTrans, invertTrans, magnitude, lerp

# Boxes
@docs BBox, OffsetDimsBox, Dims, bbox2offsetDims
-}

type alias Point = (Float, Float)

type Transform
    = Translate Float Float
    | Rotate Float
    | Scale Float

applyTrans : Transform -> Point -> Point
applyTrans trans (x, y) = 
  case trans of
    Scale s -> (x*s, y*s)
    Rotate angle -> let c = cos angle
                        s = sin angle
                    in (c*x - s*y, s*x + c*y)
    Translate tx ty -> (x + tx, y + ty)

magnitude : Point -> Float
magnitude (x, y) = sqrt <| (x^2) + (y^2)

invertTrans : Transform -> Transform
invertTrans t = case t of
                  Rotate angle -> Rotate (-angle)
                  Scale factor -> Scale (1/factor)
                  Translate x y -> Translate (-x) (-y)        

-- linear interpolation
{-| linear interpolation. To map x from interval (imin, imax) to (omin, omax), use:

    lerp (omin, omax) (imin, imax) x

-}
lerp : (Float, Float) -> (Float, Float) -> Float -> Float
lerp (omin, omax) (imin, imax) input = omin + (omax - omin) * (input - imin) / (imax - imin)

type alias BBox = { up : Float, down : Float, left : Float, right : Float }
type alias OffsetDimsBox = { offset : (Float, Float), dims : Dims } -- TODO: translate is a vector (?)
type alias Dims = { width : Float, height : Float }

bbox2offsetDims : BBox -> OffsetDimsBox
bbox2offsetDims bbox = { offset = ((bbox.right - bbox.left)/2, (bbox.up - bbox.down)/2)
                       , dims = { width = bbox.right + bbox.left
                                , height = bbox.up + bbox.down
                                }
                       }

-- vec vs point
pointAdd : Point -> Point -> Point
pointAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pointNegate : Point -> Point
pointNegate (x, y) = (-x, -y)

pointSubtract : Point -> Point -> Point
pointSubtract a b = a `pointAdd` (pointNegate b)
