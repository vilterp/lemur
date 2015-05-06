module Main where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal as S
import Debug
import Dict as D
import Maybe

import Diagrams.Geom exposing as DG
import Diagrams.Wiring as DW
import Diagrams.Interact as DI

import GraphEditor as GE
import GraphEditor.Model as GEM
import GraphEditor.Controller as GEC
import Model as M

-- MODEL

-- TODO: tabs, multiple modules
type alias State =
    { mod : Module
    , editingFn : M.FuncName
    , graphEditorState : GE.State
    , elemPanelFilter : String
    }

-- CONTROLLER

update : Action -> State -> State
update action state =
    case action of
      FilterElemPanel filter ->
          { state | elemPanelFilter <- filter }
      WindowDimsChange newDims ->
          let geState = state.graphEditorState
          in { state | graphEditorState <- { geState | collageDims <- newDims } }
      CanvasMouseEvt primMouseEvt ->
          let actions = DI.processMouseEvent state.graphEditorState.diagram
                                             state.graphEditorState.mouseState
                                             primMouseEvt
              process action state =
                  case action of
                    InternalAction act ->
                        { state | graphEditorState <- GEC.update act state.graphEditorState }
                    ExternalAction act ->
                        update act state
          in L.foldr process state actions
      GraphEditorEvt action ->
          { state | graphEditorState <- GEC.update action state.graphEditorState }
      -- add and remove
      AddLambda -> state -- TODO
      AddApNode Model.FuncId -> state -- TODO
      RemoveNode nodePath ->
          updateGraph state <| M.removeNode nodePath
      AddEdge edge ->
          updateGraph { state | dragState <- Nothing } <| M.addEdge edge
      RemoveEdge edge ->
          updateGraph state <| M.removeEdge edge
      DropNodeInLambda {lambdaPath, droppedNodePath, posInLambda} ->
          if M.canBeDroppedInLambda (state |> getGraph) lambdaPath droppedNodePath
          then updateGraph state <| M.moveNodeToLambda lambdaPath droppedNodePath posInLambda -- TODO: posInLambda not right; it's jumping
          else state
      -- 
      NoOp -> state


getUserFunc : State -> UserFuncAttrs
getUserFunc state =
    case state.mod.userFuncs |> D.get state.editingFn of
      Just (UserFunc attrs) -> attrs
      Just (BuiltinFunc _) -> Debug.crash "builtin func supposed to be user func"
      Nothing -> Debug.crash <| "no func found named " ++ state.editingFn

getGraph : State -> Graph
getGraph state =
    getUserFunc state |> .graph

updateGraph : State -> (Graph -> Result String Graph) -> State
updateGraph state updateFun =
    let mod = state.mod
        newUFs = state.mod.userFuncs
                  |> D.update state.funcName (\uFunc ->
                      Maybe.map (\(UserFunc attrs) ->
                          UserFunc { attrs | graph <- updateFun attrs.graph |> getOrCrash }) uFunc)
    in { state | mod <- { mod | userFuncs <- newUFs } }


-- VIEW

view : S.Address Update -> State -> Html
view updates state =
    let elemPanelUpdates = S.forwardTo updates ElemPanelUpdate
    in div
        [ id "app" ]
        [ topSection
        , ActionBar.view updates state
        , ElementsPanel.view elemPanelUpdates state.sidebarState
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
      , div [ class "breadcrumbs" ]
          [ div [ class "breadcrumb" ] [ text "EasySIM" ]
          , div [ class "breadcrumb breadcrumb-sep" ] [ text ">" ]
          , div [ class "breadcrumb" ]
              [ elementLabel { typ = Workflow, name = "Demo" } ]
          ]
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
    div
      [ id "center" ]
      [ div
          [ class "panel-header tabs" ]
          [ tab { typ = Workflow, name = "Demo" } True
          , tab { typ = App, name = "DSSAT" } False
          , tab { typ = Datatype, name = "PSims Weather" } False
          ]
      , div
          [ class "center-scroll" ]
          [ div
              [ id "canvas-viewport"
              , style [("width", "100%"), ("height", "100%")]
              ]
              [GraphEditor.view state.graphState]
          ]
      ]

tab : ModuleElement -> Bool -> Html
tab elem selected =
  let className = if selected then "tab selected" else "tab"
  in div [ class className ] [ elementLabel elem ]

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

htmlUpdates = S.mailbox Model.NoOp

windowUpdates = S.map Model.WindowDimsChange DW.floatWindowDims

updates = S.merge windowUpdates htmlUpdates.signal

state : Signal State
state = S.foldp Shell.update Shell.initState updates

main : Signal Html
main =
  S.map (Shell.view htmlUpdates.address) state
