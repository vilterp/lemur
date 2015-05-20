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

type DoneCallTree = 
  DoneCallTree DoneCallTreeAttrs

type FunctionId
  = LambdaId Int
  | NamedFuncId String -- TODO: use FuncId from Model ...

type alias DoneCallTreeAttrs =
  { apId : ApId
  , args : Record
  , results : Record
  , children : List DoneCallTree
  }

-- TODO: really this should just be a list
type RunningCallTree
  = RunningCallTree
      { apId : ApId
      , args : Record
      , doneChildren : List DoneCallTree
      , parent : RunningCallTree
      }
  | RunningRoot

-- N.B.: this doesn't support parallel languages like Swift
type ExecutionUpdate
    = StartCall { apId : ApId, args : Record }
    | EndCall { results : Record }

pushCall : ApId -> Record -> RunningCallTree -> RunningCallTree
pushCall apId args runningTree =
    RunningCallTree
      { apId = apId
      , args = args
      , doneChildren = []
      , parent = runningTree
      }

type RunState
  = InProgress RunningCallTree
  | DoneRunning DoneCallTree

popCall : Record -> RunningCallTree -> RunState
popCall results (RunningCallTree runningTree) =
    let doneTree =
          DoneCallTree
            { apId = runningTree.apId
            , args = runningTree.args
            , results = results
            , children = runningTree.doneChildren
            }
    in case runningTree.parent of
        RunningCallTree parent ->
            InProgress <|
              RunningCallTree { parent | doneChildren <- parent.doneChildren ++ [doneTree] }
        RunningRoot ->
            DoneRunning doneTree

processUpdate : ExecutionUpdate -> RunState -> RunState
processUpdate update state =
    case state of
      DoneRunning _ -> Debug.crash "too many updates; already done."
      InProgress runningTree ->
          case update of
            StartCall { apId, args } ->
                runningTree |> pushCall apId args |> InProgress
            EndCall { results } ->
                runningTree |> popCall results

-- TODO: use Result instead of debug.crash... (?)
buildTree : List ExecutionUpdate -> DoneCallTree
buildTree updates =
    let result = L.foldl processUpdate (InProgress RunningRoot) updates
    in case result of
        DoneRunning doneTree -> doneTree
        InProgress rt ->
            Debug.crash <| "need more done call messages. state=" ++ (toString rt) ++ " remaining=" ++ (toString updates)


