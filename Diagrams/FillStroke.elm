module Diagrams.FillStroke where

{-|
@docs FillStroke, justFill, justStroke, fillAndStroke, invisible, justSolidFill, withAlpha
-}

import Graphics.Collage as C
import Color

type alias FillStroke = { fill : Maybe C.FillStyle
                        , stroke : Maybe C.LineStyle
                        }

justFill : C.FillStyle -> FillStroke
justFill fs = { fill = Just fs, stroke = Nothing }

justStroke : C.LineStyle -> FillStroke
justStroke ls = { fill = Nothing, stroke = Just ls }

fillAndStroke : C.FillStyle -> C.LineStyle -> FillStroke
fillAndStroke fs ls = { fill = Just fs, stroke = Just ls }

invisible : FillStroke
invisible = { fill = Nothing, stroke = Nothing }

defaultStroke : C.LineStyle
defaultStroke = let defLine = C.defaultLine
                in { defLine | width <- 3
                             , cap <- C.Padded }

justSolidFill : Color.Color -> FillStroke
justSolidFill color = justFill <| C.Solid color

halfStrokeWidth : FillStroke -> Float
halfStrokeWidth fs = case fs.stroke of
                      Just ls -> ls.width/2
                      Nothing -> 0

-- should be in color lib

withAlpha : Color.Color -> Float -> Color.Color
withAlpha c a' = case c of
                  Color.RGBA r g b a -> Color.RGBA r g b a'
                  Color.HSLA h s l a -> Color.HSLA h s l a'
