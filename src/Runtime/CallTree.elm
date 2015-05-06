module Runtime.CallTree where

import Runtime.Value exposing (..)

import Dict as D
import List as L
import Result as R
import Debug

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

type PopResult
  = StillRunning RunningCallTree
  | DoneRunning DoneCallTree

popCall : Record -> RunningCallTree -> PopResult
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
            StillRunning <|
              RunningCallTree { parent | doneChildren <- parent.doneChildren ++ [doneTree] }
        RunningRoot ->
            DoneRunning doneTree

-- TODO: use Result instead of debug.crash... (?)
buildTree : List ExecutionUpdate -> DoneCallTree
buildTree updates =
    let pushOrPop update state =
          case state of
            DoneRunning _ -> Debug.crash "too many updates; already done."
            StillRunning runningTree ->
                case update of
                  StartCall { apId, args } ->
                      runningTree |> pushCall apId args |> StillRunning
                  EndCall { results } ->
                      runningTree |> popCall results
        result = L.foldl pushOrPop (StillRunning RunningRoot) updates
    in case result of
        DoneRunning doneTree -> doneTree
        StillRunning rt ->
            Debug.crash <| "need more done call messages. state=" ++ (toString rt) ++ " remaining=" ++ (toString updates)
