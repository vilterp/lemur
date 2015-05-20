module ActionBar where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal as S
import List as L
import Dict as D

import Model
import Util exposing (..)

import Debug

type alias State = List ButtonGroup
type alias ButtonGroup = List Button
type alias Button =
    { name : String
    , action : Action
    }
type Action
    = CodeAction CodeReq
    | NormalAction Model.Action

getABState : Model.State -> State
getABState state =
    [ geToolsSection, runSection ]
      |> L.filterMap (\sec -> sec state)

geToolsSection : Model.State -> Maybe ButtonGroup
geToolsSection state =
    case state.viewState of
      ViewingGraph attrs ->
          case attrs.mode of
            EditingMode ->
                Just [ { name = "Add Lambda"
                       , action = NormalAction Model.AddLambda
                       }
                     ]
            ViewingRunMode _ ->
                Nothing
      EditingBuiltin _ ->
          Nothing

runSection : Model.State -> Maybe ButtonGroup
runSection state =
    -- only show run button if we are editing a graph with no free in ports
    -- TODO: ideally button would be greyed out if there are free in ports,
    -- and hovering would explain that you need to eliminate in ports.
    case state.viewState of
      ViewingGraph attrs ->
          -- function to do this should be in model somewhere
          let graph = mod.userFuncs
                        |> D.get attrs.name
                        |> getMaybeOrCrash "no such user func"
                        |> (\UserFunc attrs -> attrs.graph)
          in case attrs.mode of
            EditingMode ->
                case Model.freeInPorts state.mod graph [] of
                  [] ->
                      let codeReq =
                            { runId = state.nextRunId
                            , mod = state.mod
                            , mainName = attrs.name
                            }
                      in Just [ { name = "Run"
                                , action = CodeAction codeReq
                                }
                              ]
                  _ -> Nothing
            ViewingRunMode ->
                Nothing
      EditingBuiltin _ ->
          Nothing

view : S.Address Model.Action -> S.Address CodeReq -> Model.State -> Html
view actionAddr codeAddr state =
    getABState state
      |> L.map (viewButtonGroup actionAddr codeAddr state)
      |> L.intersperse buttonGroupSep
      |> div [ id "action-bar" ]

viewButtonGroup : S.Address Model.Action -> S.Address CodeReq -> Model.State -> ButtonGroup -> Html
viewButtonGroup actionAddr codeAddr state bg =
    L.map (viewButton actionAddr codeAddr state) bg
      |> div [ class "action-bar-button-group" ]

-- Later: KB shortcut (for display), icon, callback (?)
viewButton : S.Address Model.Action -> S.Address CodeReq -> Model.State -> Button -> Html
viewButton actionAddr codeAddr state button =
    let actionAttr =
          case button.action of
            CodeAction codeReq ->
                onClick codeAddr <| Just codeReq
            NormalAction action ->
                onClick actionAddr action
    in div
        [ class "actionbar-button"
        , actionAttr
        ]
        [ text button.name ]

buttonGroupSep : Html
buttonGroupSep = div [ class "actionbar-vertsep" ] []
