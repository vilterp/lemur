module Runtime.Decode where

import Json.Decode as Json exposing (..)

import Runtime.Model as RM

updateList = list update

update : Decoder RM.ExecutionUpdate
update =
    ("msg" := string) `andThen` updateAttrs

updateAttrs : String -> Decoder RM.ExecutionUpdate
updateAttrs tag =
    case tag of
      "start_call" ->
          object3 (\apId path args -> RM.StartCall
                                          { apId = apId
                                          , args = args
                                          , fromFramePath = path
                                          })
            ("ap_id" := string)
            ("path" := list string)
            ("args" := record ())
      "end_call" ->
          object2 (\rfp results -> RM.EndCall
                                      { returningFramePath = rfp
                                      , results = results
                                      })
            ("path" := list string)
            ("results" := record ())

record : () -> Decoder RM.Record
record _ =
    dict (taggedValue ())

-- TODO: tagging everything like this makes the JSON horribly verbose.
-- can't think of a way to disambiguate file paths otherwise, though.
taggedValue : () -> Decoder RM.Value
taggedValue _ =
    ("tag" := string) `andThen` value

value : String -> Decoder RM.Value
value tag =
    let getVal constructor decoder =
          object1 constructor ("value" := decoder)
    in case tag of
        "int" -> getVal RM.IntVal int
        "string" -> getVal RM.StringVal string
        "list" -> getVal RM.ListVal <| list (taggedValue ())
        "record" -> getVal RM.RecordVal <| record ()
        "file" -> getVal RM.FileVal <| string
        "function" -> succeed RM.FunctionVal
