module Runtime.Model where

import Dict as D
import List as L
import Result as R
import Debug

type alias Record = D.Dict String Value

type alias FilePath = String

type Value
  = IntVal Int
  | StringVal String
  | ListVal (List Value)
  | RecordVal Record
  | FileVal FilePath
  | FunctionVal

type alias ApId = String

type FunctionId
  = LambdaId Int
  | NamedFuncId String -- TODO: use FuncId from Model ...

type CallTree = 
  CallTree CallTreeAttrs

type alias CallTreeAttrs =
  { args : Record
  , results : Maybe Record -- if nothing: active
  , children : D.Dict ApId CallTree
  }

-- N.B.: this doesn't support parallel languages like Swift
type ExecutionUpdate
    = StartCall
        { fromFramePath : List ApId
        , apId : ApId
        , args : Record
        }
    | EndCall
        { returningFramePath : List ApId
        , results : Record
        }

emptyCallTree : CallTree
emptyCallTree =
    CallTree { args = D.empty
             , results = Nothing
             , children = D.empty
             }

isDone : CallTree -> Bool
isDone (CallTree attrs) =
    case attrs.results of
      Just _ -> True
      _ -> False

processUpdate : ExecutionUpdate -> CallTree -> CallTree
processUpdate update tree =
    case update of
      StartCall startAttrs ->
          let subTree = CallTree { args = startAttrs.args
                                 , results = Nothing
                                 , children = D.empty
                                 }
              updateFun attrs =
                  { attrs | children = attrs.children
                                          |> D.insert startAttrs.apId subTree }
          in tree |> nestedTreeUpdate startAttrs.fromFramePath updateFun
      EndCall endAttrs ->
          let updateFun attrs =
                  { attrs | results = Just endAttrs.results }
          in tree |> nestedTreeUpdate endAttrs.returningFramePath updateFun

nestedTreeUpdate : List ApId -> (CallTreeAttrs -> CallTreeAttrs) -> CallTree -> CallTree
nestedTreeUpdate apPath updateFun (CallTree tree) =
    case apPath of
      [] ->
          CallTree <| updateFun tree
      (x::xs) ->
          case tree.children |> D.get x of
            Just subTree ->
                CallTree { tree | children = tree.children
                                      |> D.insert x (subTree |> nestedTreeUpdate xs updateFun) }
            Nothing ->
                Debug.crash <| "invalid update: no such subtree: " ++ x ++ " in " ++ (toString tree.children)
