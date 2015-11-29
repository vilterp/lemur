module ActionBar where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal as S
import List as L
import Dict as D

import Model exposing (..)
import Model.Graph exposing (..)
import Util exposing (..)

import Debug

type alias ABState = List ButtonGroup
type alias ButtonGroup = List Button
type alias Button =
    { name : String
    , action : Action
    }

getABState : State -> ABState
getABState state =
    [ geToolsSection, runSection ]
      |> L.filterMap (\sec -> sec state)

geToolsSection : State -> Maybe ButtonGroup
geToolsSection state =
    case state.viewState of
      ViewingGraph attrs ->
          case attrs.mode of
            EditingMode ->
                Just [ { name = "Add Lambda"
                       , action = GraphAction AddLambda
                       }
                     ]
            ViewingRunMode _ ->
                Nothing
      EditingBuiltin _ ->
          Nothing

runSection : State -> Maybe ButtonGroup
runSection state =
    -- only show run button if we are editing a graph with no free in ports
    -- TODO: ideally button would be greyed out if there are free in ports,
    -- and hovering would explain that you need to eliminate in ports.
    case state.viewState of
      ViewingGraph attrs ->
          -- function to do this should be in model somewhere
          let graph = state.mod.userFuncs
                        |> D.get attrs.name
                        |> getMaybeOrCrash "no such user func"
                        |> (\func -> case func of
                              UserFunc attrs -> attrs.graph
                              _ -> Debug.crash "only expected user funcs here")
          in case attrs.mode of
            EditingMode ->
                case freeInPorts state.mod graph [] of
                  [] ->
                      let codeReq =
                            { runId = state.nextRunId
                            , mod = state.mod
                            , mainName = attrs.name
                            }
                      in Just [ { name = "Run"
                                , action = RunCode codeReq
                                }
                              ]
                  _ -> Nothing
            ViewingRunMode _ ->
                Nothing
      EditingBuiltin _ ->
          Nothing

view : S.Address Action -> State -> Html
view addr state =
    getABState state
      |> L.map (viewButtonGroup addr)
      |> L.intersperse buttonGroupSep
      |> div [ id "action-bar" ]

viewButtonGroup : S.Address Action -> ButtonGroup -> Html
viewButtonGroup addr bg =
    L.map (viewButton addr) bg
      |> div [ class "action-bar-button-group" ]

-- Later: KB shortcut (for display), icon, callback (?)
viewButton : S.Address Action -> Button -> Html
viewButton addr button =
  div
    [ class "actionbar-button"
    , onClick addr button.action
    ]
    [ text button.name ]

buttonGroupSep : Html
buttonGroupSep = div [ class "actionbar-vertsep" ] []
