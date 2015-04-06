module Diagrams.Envelope where

{-|
@docs Direction, envelope, width, height, boundingBox
-}

import List as L
import Graphics.Element as E

import Diagrams.Core (..)
import Diagrams.Geom (..)
import Diagrams.FillStroke (..)

type Direction = Up | Down | Left | Right

{-| Given a Diagram t and a Direction, return the distance in that direction from the origin
to the closest line which doesn't intersect the content of the diagram.

 [hd]: http://projects.haskell.org/diagrams/doc/manual.html#envelopes-and-local-vector-spaces
-}
envelope : Direction -> Diagram t a -> Float
envelope dir dia =
    let handleBox w h borderWidth =
          let base = case dir of
                       Up -> h/2
                       Down -> h/2
                       Left -> w/2
                       Right -> w/2
          in base + borderWidth
    in case dia of
        Tag _ _ dia' -> envelope dir dia'
        Group dias -> case dias of -- TODO: cache
                        [] -> 0
                        _ -> L.maximum <| L.map (envelope dir) dias
        TransformD (Scale s) diag -> s * (envelope dir diag)
        TransformD (Rotate r) rotDia ->
            case rotDia of
              Path points fs pt -> let newPoints = L.map (applyTrans <| Rotate r) points
                                   in envelope dir <| Path newPoints fs pt
              Circle _ _ -> envelope dir rotDia
              -- TODO: handleBox for rect, text
        TransformD (Translate tx ty) diag -> let env = envelope dir diag
                                             in case dir of
                                                  Up -> max 0 <| env + ty
                                                  Down -> max 0 <| env - ty
                                                  Right -> max 0 <| env + tx
                                                  Left -> max 0 <| env - tx
        Text str ts te -> handleBox (toFloat <| E.widthOf te) (toFloat <| E.heightOf te) 0
        Path path fs _ -> let xs = L.map fst path
                              ys = L.map snd path
                          in case dir of
                               Left -> -(L.minimum xs)
                               Right -> L.maximum xs
                               Up -> L.maximum ys
                               Down -> -(L.minimum ys)
        Rect w h fs -> handleBox w h (halfStrokeWidth fs)
        Circle r fs -> r + (halfStrokeWidth fs)

width : Diagram t a -> Float
width d = (envelope Left d) + (envelope Right d)

height : Diagram t a -> Float
height d = (envelope Up d) + (envelope Down d)

boundingBox : Diagram t a -> BBox
boundingBox dia = { up = envelope Up dia
                  , down = envelope Down dia
                  , left = envelope Left dia
                  , right = envelope Right dia
                  }
