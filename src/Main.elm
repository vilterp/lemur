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

-- TODO: overhaul this
upGraphAndRender : Module -> FuncName -> (Graph -> Result String Graph) -> State
upGraphAndRender state upFun =
    let newState = updateGraph state upFun
        geState = newState.graphEditorState
    in { newState | graphEditorState <- { geState | diagram <- GE.render newState } }

update : Action -> State -> State
update action state =
    case Debug.log "action" action of
      NoOp -> state
      -- editor stuff
      FilterElemPanel filter ->
          { state | elemPanelFilter <- filter }
      OpenUDF funcName ->
          { state | viewState <- ViewingGraph <| EditingUDF { name = funcName
                                                            , graphEditorState = GE.initState } }
      OpenBuiltin funcName ->
          { state | viewState <- EditingBuiltin { name = funcName } }
      OpenRun runId ->
          { state | viewState <- ViewingGraph <| ViewingRun { runId = runId
                                                            , graphEditorState = GE.initState } }
      -- running
      StartExecution ->
          state |> addNewRun
      ExecutionUpdate runId update ->
          state |> processExecutionUpdate runId update
      -- graph ops
      CanvasMouseEvt (collageLoc, primMouseEvt) ->
          -- TODO: save collage loc
          
          let (newMS, actions) =
                  DI.processMouseEvent 
                      state.graphEditorState.diagram
                      state.graphEditorState.mouseState
                      primMouseEvt
              geState = state.graphEditorState
              newMSState = { state | graphEditorState <- { geState | mouseState <- newMS
                                                                   , collageLoc <- collageLoc } }
              process : GraphEditorAction -> State -> State
              process act state =
                    case Debug.log "ge act" act of
                      InternalAction intAct ->
                          GE.update intAct state
                      ExternalAction extAct ->
                          update extAct state
              newState = L.foldl process newMSState actions
              newGEState = newState.graphEditorState
          in { newState | graphEditorState <- { newGEState | diagram <- GE.render newState } }
      GraphAction graphAction ->
          case state.viewState of
            ViewingGraph graphViewState ->
                case graphViewState of
                  EditingUDF attrs ->
                      -- TODO: put this in GraphEditor's update function
                      case graphAction of
                        MoveNode nodePath point ->
                            upGraphAndRender state <| moveNode nodePath point
                        AddLambda ->
                            let (newState, lambdaIdNo) = getApId state
                                lambdaId = "lambda" ++ toString lambdaIdNo
                                posNode = { pos = defaultPos
                                          , id = lambdaId
                                          , node = emptyLambdaNode
                                          }
                            in upGraphAndRender newState <| addNode [lambdaId] posNode
                        AddApNode funcId ->
                            let (newState, apIdNo) = getLambdaId state
                                apId = "ap" ++ toString apIdNo
                                posNode = { pos = defaultPos
                                          , id = apId
                                          , node = ApNode funcId
                                          }
                            in upGraphAndRender newState <| addNode [apId] posNode
                        RemoveNode nodePath ->
                            upGraphAndRender state <| removeNode nodePath
                        AddEdge edge ->
                            upGraphAndRender state <| addEdge edge
                        RemoveEdge edge ->
                            upGraphAndRender state <| removeEdge edge
                        DropNodeInLambda {lambdaPath, droppedNodePath, posInLambda} ->
                            if canBeDroppedInLambda (state |> getCurrentGraph) lambdaPath droppedNodePath
                            then upGraphAndRender state <| moveNodeToLambda lambdaPath droppedNodePath posInLambda -- TODO: posInLambda not right; it's jumping
                            else state
                  ViewingRun _ ->
                    Debug.crash "graph action while viewing run"
            EditingBuiltin _ ->
                Debug.crash "graph action while editing builtin"

defaultPos = (0, 0)

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
                let mainView = GE.view state
                in case attrs.mode of
                  EditingMode ->
                      ( udfIcon
                      , attrs.name
                      , mainView
                      )
                  ViewingRunMode runId ->
                      ( runIcon
                      , runLabel ( attrs.runId
                                 , attrs.runId |> getRunOrCrash state
                                 )
                      , mainView
                      )
            EditingBuiltin attrs ->
                ( builtinIcon
                , attrs.name
                , state.mod.builtinFuncs
                    |> D.get attrs.name
                    |> getMaybeOrCrash "no such builtin function"
                    |> (\BuiltinFunc attrs -> builtinView attrs)
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
              [ builtinAttrs.pythonCode ]
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
          , graphEditorState = GE.initState -- TODO check on this
          , mode = EditingMode
          }
    , nextRunId = 1
    , runs = D.empty
    }

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
          S.send htmlUpdates.address StartExecution
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
