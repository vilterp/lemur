module Runtime.DecodeTest where

import Dict
import List
import Json.Decode
import Runtime.Decode
import Runtime.Value exposing (..)

testVal =
  [ "{\"tag\": \"int\", \"value\": 2}"
  , "{\"tag\": \"string\", \"value\": \"foo\"}"
  , "{\"tag\": \"file\", \"value\": \"foo/bar.txt\"}"
  , "{\"tag\": \"list\", \"value\": [{\"tag\": \"int\", \"value\": 2}, {\"tag\": \"int\", \"value\": 3}]}"
  , "{\"tag\": \"record\", \"value\": {\"foo\": {\"tag\": \"int\", \"value\": 2}, \"bar\": {\"tag\": \"int\", \"value\": 3}}}"
  ]
  |> List.map (Json.Decode.decodeString (Runtime.Decode.taggedValue ()))

correctVal =
  [ Ok (IntVal 2)
  , Ok (StringVal "foo")
  , Ok (FileVal "foo/bar.txt")
  , Ok (ListVal ([IntVal 2,IntVal 3]))
  , Ok (RecordVal (Dict.fromList [("bar",IntVal 3), ("foo",IntVal 2)]))
  ]

testUpdates =
  "[{\"msg\": \"start_call\", \"ap_id\": 0, \"args\": {\"n\": {\"tag\": \"int\", \"value\": 2}}}, {\"msg\": \"start_call\", \"ap_id\": 1, \"args\": {\"n\": {\"tag\": \"int\", \"value\": 1}}}, {\"msg\": \"end_call\", \"results\": {\"result\": {\"tag\": \"int\", \"value\": 1}}}, {\"msg\": \"start_call\", \"ap_id\": 2, \"args\": {\"n\": {\"tag\": \"int\", \"value\": 0}}}, {\"msg\": \"end_call\", \"results\": {\"result\": {\"tag\": \"int\", \"value\": 1}}}, {\"msg\": \"end_call\", \"results\": {\"result\": {\"tag\": \"int\", \"value\": 2}}}]"
    |> Json.Decode.decodeString Runtime.Decode.updateList
