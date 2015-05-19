module Runtime.DecodeTest where

import Dict
import List
import Json.Decode
import Runtime.Decode
import Runtime.Value exposing (..)
import Runtime.ViewValue exposing (..)

import Diagrams.Core exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Pad exposing (..)

import Color

import CommonView exposing (..)

testVal =
  [ "{\"tag\": \"int\", \"value\": 2}"
  , "{\"tag\": \"string\", \"value\": \"foo\"}"
  , "{\"tag\": \"file\", \"value\": \"foo/bar.txt\"}"
  , "{\"tag\": \"list\", \"value\": [{\"tag\": \"int\", \"value\": 2}, {\"tag\": \"int\", \"value\": 3}]}"
  , "{\"tag\": \"record\", \"value\": {\"foo\": {\"tag\": \"int\", \"value\": 2}, \"bar\": {\"tag\": \"int\", \"value\": 3}}}"
  ]
  |> List.map (Json.Decode.decodeString (Runtime.Decode.taggedValue ()))

correctVals =
    [ (IntVal 2)
    , (StringVal "foo")
    , (FileVal "foo/bar.txt")
    , (ListVal ([IntVal 2,IntVal 3]))
    , (RecordVal (Dict.fromList [("bar",IntVal 3), ("foo",IntVal 2)]))
    ]

correctResults = List.map Ok correctVals

testUpdates =
  "[{\"msg\": \"start_call\", \"ap_id\": 0, \"args\": {\"n\": {\"tag\": \"int\", \"value\": 2}}}, {\"msg\": \"start_call\", \"ap_id\": 1, \"args\": {\"n\": {\"tag\": \"int\", \"value\": 1}}}, {\"msg\": \"end_call\", \"results\": {\"result\": {\"tag\": \"int\", \"value\": 1}}}, {\"msg\": \"start_call\", \"ap_id\": 2, \"args\": {\"n\": {\"tag\": \"int\", \"value\": 0}}}, {\"msg\": \"end_call\", \"results\": {\"result\": {\"tag\": \"int\", \"value\": 1}}}, {\"msg\": \"end_call\", \"results\": {\"result\": {\"tag\": \"int\", \"value\": 2}}}]"
    |> Json.Decode.decodeString Runtime.Decode.updateList


main =
    correctVals
      |> List.map (view >> alignCenter >> pad 2 >>  tooltip defaultTooltipSettings)
      |> List.intersperse (hspace 5)
      |> hcat
      |> fullWindowMain
