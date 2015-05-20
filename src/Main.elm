module Main where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal as S
import Debug
import Dict as D
import List as L
import Maybe
import Task as T

import Diagrams.Core as DC
import Diagrams.Geom as DG
import Diagrams.Wiring as DW
import Diagrams.Interact as DI

import Model exposing (..)
import GraphEditor.Model exposing (..)
import Util exposing (..)

import ElementsPanel
import ActionBar
import GraphEditor as GE
import CommonView exposing (..)

import TestData

import Codegen
import Runtime.Decode
import Runtime.CallTree
import Http

-- MODEL

-- TODO: tabs, multiple modules

-- CONTROLLER

update : Action -> State -> State
update action state =
    case Debug.log "action" action of
      NoOp -> state
      -- editor stuff
      FilterElemPanel filter ->
          { state | elemPanelFilter <- filter }
      OpenUDF funcName ->
          { state | viewState <- ViewingGraph { name = funcName
                                              , editorState = GE.initState
                                              , mode = EditingMode
                                              } }
              |> renderState
      OpenBuiltin funcName ->
          { state | viewState <- EditingBuiltin { name = funcName } }
      OpenRun runId ->
          let run = state |> getRunOrCrash runId
          in { state | viewState <- ViewingGraph { name = run.userFuncName
                                                 , editorState = GE.initState
                                                 , mode = ViewingRunMode runId
                                                 } }
                |> renderState
      -- running
      StartExecution funcName ->
          state
            |> addNewRun funcName
            |> update (OpenRun <| state.nextRunId)
      ExecutionUpdate runId update ->
          state |> processExecutionUpdate runId update
      -- graph ops
      CanvasMouseEvt (collageLoc, primMouseEvt) ->
          -- TODO: save collage loc
          case state.viewState of
            ViewingGraph graphViewAttrs ->
                let clState = { state | collageLoc <- collageLoc }
                    (newMS, actions) =
                        DI.processMouseEvent 
                            graphViewAttrs.editorState.diagram
                            graphViewAttrs.editorState.mouseState
                            primMouseEvt
                    process : GraphEditorAction -> GraphViewModel -> GraphViewModel
                    process act viewModel =
                        case Debug.log "ge act" act of
                          InternalAction intAct ->
                              GE.update intAct viewModel
                          ExternalAction graphAction ->
                              GE.updateGraph graphAction viewModel
                    newViewModel =
                        L.foldl process (makeViewModel clState) actions
                          |> GE.render
                    newEditorState = newViewModel.editorState
                    mis = newEditorState.mouseInteractionState
                in { clState | mod <- newViewModel.mod
                             , viewState <- ViewingGraph
                                  { graphViewAttrs | editorState <-
                                                        { newEditorState | mouseState <- newMS } } }
            EditingBuiltin _ ->
                let l = Debug.log "canvas action while editing builtin" ()
                in state
      GraphAction graphAction ->
          case state.viewState of
            ViewingGraph graphViewAttrs ->
                case graphViewAttrs.mode of
                  EditingMode ->
                      -- TODO: put this in GraphEditor's update function
                      -- GE.updateGraph graphAction (makeViewModel state)
                      let newViewModel =
                              GE.updateGraph graphAction (makeViewModel state)
                                |> GE.render
                          newViewAttrs = { graphViewAttrs | editorState <- newViewModel.editorState }
                      in { state | viewState <- ViewingGraph newViewAttrs
                                 , mod <- newViewModel.mod }
                  ViewingRunMode _ ->
                    let l = Debug.log "graph action while viewing run" ()
                    in state
            EditingBuiltin _ ->
                let l = Debug.log "graph action while editing builtin" ()
                in state

defaultPos = (0, 0)

renderState : State -> State
renderState state =
    case state.viewState of
      ViewingGraph attrs ->
          let vm = makeViewModel state |> GE.render
          in { state | viewState <- ViewingGraph { attrs | editorState <- vm.editorState } }
      EditingBuiltin _ ->
          Debug.crash "unexpected usage of renderState"

-- VIEW

view : State -> Html
view state =
    div
      [ id "app" ]
      [ topSection
      , ActionBar.view htmlUpdates.address codeExecutionRequests.address state
      , ElementsPanel.view htmlUpdates.address state
      , centerSection state
      , rightSection
      ]

-- TODO: probably split each section into its own module with model, view, and controller

-- TOP SECTION

topSection : Html
topSection =
    div
      [ id "top" ]
      [ div [ id "logo" ] [ text "VisualFP" ]
      -- TODO: breadcrumbs
      , div [ id "user-state" ]
          [ div [ id "prof-pic" ] [ text "PV" ]
          , div [ id "username" ] [ text "vilterp" ]
          , div [ id "logout" ] [ text "(Logout)" ]
          ]
      ]


-- CENTER SECTION

-- TODO: tabs. move to own file.

centerSection : State -> Html
centerSection state =
    let (icon, label, mainView) =
          case state.viewState of
            ViewingGraph attrs ->
                let mainView = GE.view state.collageLoc attrs.editorState
                in case attrs.mode of
                  EditingMode ->
                      ( udfIcon
                      , attrs.name
                      , mainView
                      )
                  ViewingRunMode runId ->
                      ( runIcon
                      , runLabel ( runId
                                 , state |> getRunOrCrash runId
                                 )
                      , mainView
                      )
            EditingBuiltin attrs ->
                ( builtinIcon
                , attrs.name
                , state.mod.builtinFuncs
                    |> D.get attrs.name
                    |> getMaybeOrCrash "no such builtin function"
                    |> (\(BuiltinFunc attrs) -> builtinView attrs)
                )
    in div
      [ id "center" ]
      [ div
          [ class "panel-header" ]
          [ div [ class "element-icon" ] [ icon |> asHtml ]
          , div [ class "element-label" ] [ text label ]
          ]
      , div
          [ class "center-scroll" ]
          [ div
              [ id "canvas-viewport"
              , style [("width", "100%"), ("height", "100%")]
              ]
              [ mainView ]
          ]
      ]

-- TODO: make editable
-- TODO: show / edit type sig
builtinView : BuiltinFuncAttrs -> Html
builtinView builtinAttrs =
    div
      [ class "builtin-editor" ]
      [ div
          [ class "builtin-params" ]
          [ builtinAttrs.params |> toString |> text ]
      , div
          [ class "builtin-return-vals" ]
          [ builtinAttrs.returnVals |> toString |> text ]
      , div
          [ class "builtin-code" ]
          [ pre
              [ class "builtin-code-pre" ]
              [ builtinAttrs.pythonCode |> text ]
          ]
      ]

-- RIGHT SECTION

-- TODO: maybe unnecessary?

rightSection : Html
rightSection =
    div
      [ id "right" ]
      [ scrollPanel
          [ div [ class "panel-header-label" ] [ text "Editor" ] ]
          [ p [] [ text lipsum1 ]
          , p [] [ text lipsum2 ] 
          ]
      ]

lipsum1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laboru"
lipsum2 = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"


-- wiring

initState : State
initState =
    { mod = TestData.helloMap
    , elemPanelFilter = ""
    , collageLoc = editorLocFunc { width = 1280, height = 701 } -- specific to my 13" MBP :P
    , viewState =
        ViewingGraph
          { name = "main"
          , editorState = GE.initState -- TODO check on this
          , mode = EditingMode
          }
    , nextRunId = 1
    , runs = D.empty
    }
      |> renderState

htmlUpdates = S.mailbox Model.NoOp

editorLocFunc : DW.CollageLocFunc
editorLocFunc windowDims =
    { offset = (252, 101)
    , dims = { width = windowDims.width - 501
             , height = windowDims.height - 101
             }
    }

collageEvents = DW.makeUpdateStream editorLocFunc |> S.map CanvasMouseEvt

updates : S.Signal Action
updates = S.merge collageEvents htmlUpdates.signal

state : Signal State
state = S.foldp update initState updates

main : Signal Html
main =
  S.map view state

codeExecutionRequests : S.Mailbox (Maybe CodeReq)
codeExecutionRequests = 
    S.mailbox Nothing

port codeExecTasks : Signal (T.Task Http.Error ())
port codeExecTasks =
    codeExecutionRequests.signal
      |> S.map requestAndSend

requestAndSend : Maybe CodeReq -> T.Task Http.Error ()
requestAndSend codeReq =
    case codeReq of
      Nothing -> T.succeed ()
      Just {runId, mod, mainName} ->
          S.send htmlUpdates.address (StartExecution mainName)
            `T.andThen`
              (\_ -> requestExecution mod mainName
                `T.andThen` (sendToUpdates runId))

sendToUpdates : RunId -> List Runtime.CallTree.ExecutionUpdate -> T.Task Http.Error ()
sendToUpdates runId updates =
    updates
      |> L.map (\update -> ExecutionUpdate runId update |> S.send htmlUpdates.address)
      |> (\tasks -> T.sequence tasks
                      `T.andThen` (always <| T.succeed ()))

requestExecution : Module -> String -> T.Task Http.Error (List Runtime.CallTree.ExecutionUpdate)
requestExecution mod mainFunc =
    Http.url "/run_python" [("code", Codegen.moduleToPython mainFunc mod)]
      |> Http.get Runtime.Decode.updateList

bodyFromCode : String -> Http.Body
bodyFromCode code =
    Http.string code
