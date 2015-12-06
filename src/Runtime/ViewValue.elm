module Runtime.ViewValue where

import List as L
import Dict as D

import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Bezier exposing (..)
import Diagrams.Layout exposing (..)
import Diagrams.FillStroke exposing (..)

import Runtime.Model exposing (..)

import Text
import Graphics.Collage as C
import Color

view : Value -> Diagram t a
view value =
    case value of
      IntVal x ->
          toString x |> text intTextStyle |> padSpecific 0 0 3 3
      StringVal str ->
          str
            |> text strTextStyle
      ListVal list ->
          list
            |> L.map (view >> flexRight)
            |> L.intersperse (hrule listRuleStyle 2)
            |> layout
            |> background listFillStroke
      RecordVal record ->
          record
            |> D.toList
            |> L.map (viewKeyValue >> flexRight)
            |> L.intersperse (hrule listRuleStyle 2)
            |> layout
            |> background listFillStroke
      FileVal file ->
          text fileTextStyle file
      FunctionVal ->
          text functionTextStyle "<function>"

viewKeyValue : (String, Value) -> Diagram t a
viewKeyValue (key, value) =
    hcat [ hspace 3
         , text dictKeyStyle key
         , text dictColonStyle ":"
         , hspace 5
         , view value
         ]

-- Styles

defaultTextStyle =
    let defStyle = Text.defaultStyle
    in { defStyle | color = Color.white }

intTextStyle = { defaultTextStyle | color = Color.lightBlue }

strTextStyle = { defaultTextStyle | color = Color.green }
strFillStroke = justFill (Solid Color.green)

defaultLineStyle =
    let defStyle = C.defaultLine
    in { defStyle | color = Color.white }

listRuleStyle = defaultLineStyle
listFillStroke = justStroke defaultLineStyle

fileTextStyle = { defaultTextStyle | color = Color.red }

functionTextStyle = defaultTextStyle

dictKeyStyle = defaultTextStyle
dictColonStyle = defaultTextStyle