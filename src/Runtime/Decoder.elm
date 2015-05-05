module Runtime.Decoder where

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
          ("args" := record)
      "end_call" ->
        object1 (\results -> RCT.EndCall { results = results })
          ("results" := record)

record : Decoder RV.Record
record =
    dict value

value : Decoder RV.Value
value =
    oneOf
      [ int |> map RV.IntVal
      , string |> map RV.StringVal
      --, list value |> map RV.ListVal
      --, dict value |> map RV.RecordVal
      ]

-- don't know how to disambiguate file vals from records or lists
