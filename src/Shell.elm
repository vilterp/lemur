module Shell where

import Diagrams.Wiring as DW

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Debug

import GraphEditor
import Shell.ActionBar
import GraphEditor.TestData as GETD

-- MODEL

type Update
    = ElemPanelUpdate SidebarAction
    | GraphEditorUpdate GraphEditor.GraphEditorEvt
    | NoOp

-- TODO: move CollaGraphEditorLocation to GraphEditor state
type alias State =
    { sidebarState : SidebarState
    , graphState : GraphEditor.State
    }

initState =
    { sidebarState = emptySidebarState
    , graphState = GraphEditor.initState GETD.initState
    }

-- CONTROLLER

update : Update -> State -> State
update up state =
  case up of
    ElemPanelUpdate action ->
        { state | sidebarState <- sidebarUpdate action state.sidebarState }
    GraphEditorUpdate evt ->
        { state | graphState <- GraphEditor.update evt state.graphState }
    NoOp -> state

-- VIEW

view : S.Address Update -> State -> Html
view updates state =
    let sidebarUpdates = S.forwardTo updates ElemPanelUpdate
    in div
        [ id "app" ]
        [ topSection
        , ActionBar.view updates state
        , elementsPanelView sidebarUpdates state.sidebarState
        , centerSection state
        , rightSection
        ]

-- TODO: probably split each section into its own module with model, view, and controller

-- TOP SECTION

topSection : Html
topSection =
    div
      [ id "top" ]
      [ div [ id "logo" ] [ text "Swift/V" ]
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
