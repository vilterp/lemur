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
          ("ap_id" := string)
          ("args" := record ())
      "end_call" ->
        object1 (\results -> RCT.EndCall { results = results })
          ("results" := record ())

record : () -> Decoder RV.Record
record _ =
    dict (taggedValue ())

-- TODO: tagging everything like this makes the JSON horribly verbose.
-- can't think of a way to disambiguate file paths otherwise, though.
taggedValue : () -> Decoder RV.Value
taggedValue _ =
    ("tag" := string) `andThen` value

value : String -> Decoder RV.Value
value tag =
    let getVal constructor decoder =
          object1 constructor ("value" := decoder)
    in case tag of
        "int" -> getVal RV.IntVal int
        "string" -> getVal RV.StringVal string
        "list" -> getVal RV.ListVal <| list (taggedValue ())
        "record" -> getVal RV.RecordVal <| record ()
        "file" -> getVal RV.FileVal <| string
        "function" -> succeed RV.FunctionVal
