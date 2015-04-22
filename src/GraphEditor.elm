module GraphEditor where

import Debug

import Html
import Graphics.Collage as C

import Diagrams.Interact as DI
import Diagrams.Geom as DG
import Diagrams.Core as D
import Diagrams.Wiring as DW

import GraphEditor.Model as GEM
import GraphEditor.View as GEV
import GraphEditor.Controller as GEC
import GraphEditor.Util exposing (..)

import Model

type alias State =
    { intState : DI.InteractionState GEM.State GEM.Tag GEM.Action
    , editorLoc : DW.CollageLocation
    , lambdaId : Int
    , apId : Int
    }

type GraphEditorEvt
    = MouseUpdate (DW.CollageLocation, DW.PrimMouseEvent)
    | AddLambda
    | AddBuiltin (Model.BuiltinFunc)
    | AddUserFunc (Model.UserFunc)

initState : GEM.State -> State
initState state =
    { intState = DI.initInteractState GEC.update GEV.viewGraph state
    , lambdaId = 0
    , apId = 0
    -- BUG: this is specific to my 13" macbook pro screen, with full screen chrome.
    -- need to look at window dims on startup
    , editorLoc = editorLocFunc { width = 1280, height = 701 }
    }

{- TODO: own action type which includes actions for each button, like
- add lambda
- add list, add literal -}

-- TODO: make this in pre-panned space
defaultPos = (0, 0)

editorLocFunc : DW.CollageLocFunc
editorLocFunc windowDims =
    let d = Debug.log "dims" windowDims
    in { offset = (252, 101)
       , dims = { width = windowDims.width - 501
                , height = windowDims.height - 101
                }
       }

update : GraphEditorEvt -> State -> State
update evt state =
    case evt of
      MouseUpdate mouseEvt ->
          { state | intState <- DI.update mouseEvt state.intState }
      AddLambda ->
          let newNode = GEM.emptyLambdaNode
              newId = "lambda" ++ (toString state.lambdaId)
              newPosNode = { pos = defaultPos, id = newId, node = newNode }
              withNewNode = addNodeToState newPosNode state
          in { withNewNode | lambdaId <- state.lambdaId + 1 }
      AddBuiltin builtinFunc ->
          addFuncNode builtinFunc state
      AddUserFunc userFunc ->
          addFuncNode userFunc state

addNodeToState : GEM.PosNode -> State -> State
addNodeToState posNode state =
    { state | intState <- DI.updateModel
                            (\geState ->
                                { geState | graph <- getOrCrash <|
                                    GEM.addNode [newId] newPosNode geState.graph })
                            state.intState }

addFuncNode : Model.Func a -> State -> State
addFuncNode func state =
    let newNode = ApNode { title = func.name
                         , params = func.params
                         , results = ["result"]
                         }
        newPosNode = { pos = defaultPos
                     , id = "ap" ++ (toString state.apId)
                     , node = newNode
                     }
    in { state | graph <- addNodeToState newPosNode state
               , apId <- apId + 1 }

view : State -> Html.Html
view state =
    let dims = state.editorLoc.dims
    in [D.render state.intState.diagram]
          |> C.collage (round dims.width) (round dims.height)
          |> Html.fromElement
