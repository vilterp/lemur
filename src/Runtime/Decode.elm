module Runtime.Decode where

import Json.Decode as Json exposing (..)

import Runtime.CallTree as RCT
import Runtime.Value as RV

updateList = list update

update : Decoder RCT.ExecutionUpdate
update =
    ("msg" := string) `andThen` updateAttrs

updateAttrs : String -> Decoder RCT.ExecutionUpdate
updateAttrs tag =
    case tag of
      "start_call" ->
        object2 (\apId args -> RCT.StartCall { apId = apId, args = args })
          ("ap_id" := int)
          ("args" := record "asfd")
      "end_call" ->
        object1 (\results -> RCT.EndCall { results = results })
          ("results" := record "asf")

record : String -> Decoder RV.Record
record asdf =
    dict (taggedValue "asf")

taggedValue : String -> Decoder RV.Value
taggedValue u =
    ("tag" := string) `andThen` value

value : String -> Decoder RV.Value
value tag =
    let getVal constructor decoder =
          object1 constructor ("value" := decoder)
    in case tag of
        "int" -> getVal RV.IntVal int
        "string" -> getVal RV.StringVal string
        "list" -> getVal RV.ListVal <| list (taggedValue "asdf")
        "record" -> getVal RV.RecordVal <| record "asdf"
        "file" -> getVal RV.FileVal <| string
