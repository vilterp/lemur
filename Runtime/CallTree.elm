module Runtime.CallTree where

import Runtime.Value exposing (..)

import Dict as D
import List as L
import Debug

type alias ApId = Int

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

type RunningCallTree =
  RunningCallTree
    { apId : ApId
    , args : Record
    , doneChildren : List DoneCallTree
    , parent : Maybe RunningCallTree
    }

initRunningCallTree : ApId -> Record -> RunningCallTree
initRunningCallTree apId args =
    RunningCallTree
      { apId = apId
      , args = args
      , doneChildren = []
      , parent = Nothing
      }

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
      , parent = Just runningTree
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
        Just (RunningCallTree parent) ->
            StillRunning <|
              RunningCallTree { parent | doneChildren <- parent.doneChildren ++ [doneTree] }
        Nothing ->
            DoneRunning doneTree


buildTree : ApId -> Record -> List ExecutionUpdate -> DoneCallTree
buildTree apId args updates =
    let pushOrPop update state =
          case state of
            DoneRunning _ ->
                Debug.crash "too many updates; already done"
            StillRunning runningTree ->
                case update of
                  StartCall { apId, args } ->
                      runningTree
                        |> pushCall apId args
                        |> StillRunning
                  EndCall { results } ->
                      runningTree
                        |> popCall results
        result = L.foldl pushOrPop (initRunningCallTree apId args |> StillRunning) updates
    in case result of
        StillRunning rt -> Debug.crash <| "need more done call messages. state=" ++ (toString rt) ++ " remaining=" ++ (toString updates)
        DoneRunning doneTree -> doneTree
