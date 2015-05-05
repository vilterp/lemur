module Runtime.Value where

import Dict as D

type alias Record = D.Dict String Value

type alias FilePath = String

type Value
  = IntVal Int
  | StringVal String
  | ListVal (List Value)
  | RecordVal Record
  --| FileVal FilePath