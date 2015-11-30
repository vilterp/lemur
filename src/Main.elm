module Main where

import Signal as S
import Debug
import Dict as D
import List as L
import String
import Maybe
import Task as T
import Json.Encode as JsEnc

import Diagrams.Core as DC
import Diagrams.Geom as DG
import Diagrams.Wiring as DW
import Diagrams.Interact as DI
import Http
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Effects exposing (Effects)

import Model exposing (..)
import GraphEditor.Model exposing (..)
import Util exposing (..)
import ElementsPanel
import ActionBar
import GraphEditor as GE
import CommonView exposing (..)
import Codegen
import Runtime.Decode
import Runtime.Model
import TestData

-- MODEL

-- TODO: tabs, multiple modules

-- CONTROLLER

update : Action -> State -> (State, Effects Action)
update action state =
    case Debug.log "action" action of
      NoOp ->
        (state, Effects.none)

      -- editor stuff
      FilterElemPanel filter ->
          ( { state | elemPanelFilter = filter }
          , Effects.none
          )

      OpenGraphFunc funcName ->
          ( { state | viewState =
                ViewingGraph
                  { name = funcName
                  , editorState = GE.initState
                  , mode = EditingMode
                  }
            }
            |> renderState
          , Effects.none
          )

      OpenPythonFunc funcName ->
          ( { state | viewState = EditingPythonFunc { name = funcName } }
          , Effects.none
          )

      OpenRun runId ->
          ( let
              run =
                state |> getRunOrCrash runId
            in
              { state | viewState =
                  ViewingGraph
                    { name = run.graphFuncName
                    , editorState = GE.initState
                    , mode = ViewingRunMode runId
                    }
              }
              |> renderState
          , Effects.none
          )

      -- running
      RunCode codeReq ->
          ( let
              stateWithNewRun =
                state
                  |> addNewRun codeReq.mainName

              runId =
                state.nextRunId

              run =
                stateWithNewRun |> getRunOrCrash runId
            in
              { stateWithNewRun | viewState =
                  ViewingGraph
                    { name = run.graphFuncName
                    , editorState = GE.initState
                    , mode = ViewingRunMode runId
                    }
              }
              |> renderState
          , runCode codeReq
              |> T.mapError RunCodeError
              |> T.toResult
              |> T.map (\res ->
                  case res of
                    Ok act -> act
                    Err act -> act)
              |> Effects.task
          )

      RunCodeError error ->
        let
          d = Debug.log <| "CODE RUNNING ERROR" ++ toString error
        in
          (state, Effects.none)

      ExecutionUpdates runId updates ->
          ( List.foldl (processExecutionUpdate runId) state updates
              |> renderState
          , Effects.none
          )
            --|> renderState
      -- graph ops
      CanvasMouseEvt (collageLoc, primMouseEvt) ->
          -- TODO: save collage loc
          case state.viewState of
            ViewingGraph graphViewAttrs ->
                let
                  clState =
                    { state | collageLoc = collageLoc }

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
                  
                  newEditorState =
                    newViewModel.editorState

                  mis =
                    newEditorState.mouseInteractionState
                in
                  ( { clState
                        | mod = newViewModel.mod
                        , viewState =
                            ViewingGraph
                              { graphViewAttrs | editorState =
                                  { newEditorState | mouseState = newMS }
                              }
                    }
                  , Effects.none
                  )

            EditingPythonFunc _ ->
                let
                  l = Debug.log "canvas action while editing python func" ()
                in
                  ( state, Effects.none )

      GraphAction graphAction ->
          case state.viewState of
            ViewingGraph graphViewAttrs ->
                case graphViewAttrs.mode of
                  EditingMode ->
                      -- TODO: put this in GraphEditor's update function
                      -- GE.updateGraph graphAction (makeViewModel state)
                      let
                        newViewModel =
                          GE.updateGraph graphAction (makeViewModel state)
                            |> GE.render
                        
                        newViewAttrs =
                          { graphViewAttrs | editorState = newViewModel.editorState }
                      in
                        ( { state | viewState = ViewingGraph newViewAttrs
                                  , mod = newViewModel.mod
                          }
                        , Effects.none
                        )

                  ViewingRunMode _ ->
                    let l = Debug.log "graph action while viewing run" ()
                    in ( state, Effects.none )

            EditingPythonFunc _ ->
                let l = Debug.log "graph action while editing python func" ()
                in ( state, Effects.none )

defaultPos = (0, 0)

renderState : State -> State
renderState state =
    case state.viewState of
      ViewingGraph attrs ->
          let vm = makeViewModel state |> GE.render
          in { state | viewState = ViewingGraph { attrs | editorState = vm.editorState } }
      EditingPythonFunc _ ->
          Debug.crash "unexpected usage of renderState"

-- VIEW

view : S.Address Action -> State -> Html
view addr state =
    div
      [ id "app" ]
      [ topSection
      , ActionBar.view addr state
      , ElementsPanel.view addr state
      , centerSection state
      --, rightSection
      ]

-- TODO: probably split each section into its own module with model, view, and controller

-- TOP SECTION

topSection : Html
topSection =
    div
      [ id "top" ]
      [ div [ id "logo" ] [ text "Lemur" ]
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
            EditingPythonFunc attrs ->
                ( pythonIcon
                , attrs.name
                , state.mod.pythonFuncs
                    |> D.get attrs.name
                    |> getMaybeOrCrash "no such python function"
                    |> (\func ->
                          case func of
                            PythonFunc attrs -> pythonEditorView attrs
                            _ -> Debug.crash "only expecting python funcs here")
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
pythonEditorView : PythonFuncAttrs -> Html
pythonEditorView pythonAttrs =
    div
      [ class "python-editor" ]
      [ div
          [ class "python-func-sig" ]
          [ text <| "def " ++ pythonAttrs.name ++ "("
          , input
              [ class "code-input python-args"
              , type' "text"
              , pythonAttrs.params |> String.join ", " |> value ]
              []
          , text "): # => {"
          , input
              [ class "code-input python-returns"
              , type' "text"
              , pythonAttrs.returnVals |> String.join ", " |> value ]
              []
          , text "}"
          ]
      , div
          [ class "python-code" ]
          [ textarea
              [ class "python-code-editor"
              , spellcheck False
              ]
              [ pythonAttrs.pythonCode |> text ]
          ]
      ]

-- RIGHT SECTION

-- TODO: maybe unnecessary?

--rightSection : Html
--rightSection =
--    div
--      [ id "right" ]
--      [ scrollPanel
--          [ div [ class "panel-header-label" ] [ text "Editor" ] ]
--          [ p [] [ text lipsum1 ]
--          , p [] [ text lipsum2 ] 
--          ]
--      ]

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

editorLocFunc : DW.CollageLocFunc
editorLocFunc windowDims =
    { offset = (252, 101)
    , dims = { width = windowDims.width - 250
             , height = windowDims.height - 101
             }
    }


app : StartApp.App State
app =
  StartApp.start
    { init = (initState, Effects.none)
    , update = update
    , view = view
    , inputs = [collageEvents]
    }

collageEvents = DW.makeUpdateStream editorLocFunc |> S.map CanvasMouseEvt

main : Signal Html
main =
  app.html

port tasks : S.Signal (T.Task Effects.Never ())
port tasks =
  app.tasks

runCode : CodeReq -> T.Task Http.Error Action
runCode codeReq =
  let
    code =
      Codegen.moduleToPython codeReq.mainName codeReq.mod

    body =
      Http.string <|
        JsEnc.encode
          0
          (JsEnc.object [("code", JsEnc.string code)])
  in
    (Http.send
      Http.defaultSettings
      { verb = "POST"
      , headers = [("Content-Type", "application/json")]
      , url = "/run_python"
      , body = body
      })
    |> Http.fromJson Runtime.Decode.updateList
    |> T.map (\updates -> ExecutionUpdates codeReq.runId updates)
