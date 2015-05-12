module ActionBar where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal as S
import List as L

import Model

type alias State = List ButtonGroup
type alias ButtonGroup = List Button
type alias Button =
    { name : String
    , action : Model.Action
    }

getABState : Model.State -> State
getABState smState =
    [ [{ name = "Add Lambda", action = Model.AddLambda }]
    ]

view : S.Address Model.Action -> Model.State -> Html
view chan state =
    getABState state
      |> L.map (viewButtonGroup chan)
      |> L.intersperse buttonGroupSep
      |> div [ id "action-bar" ]

viewButtonGroup : S.Address Model.Action -> ButtonGroup -> Html
viewButtonGroup chan bg =
    L.map (viewButton chan) bg
      |> div [ class "action-bar-button-group" ]

-- Later: KB shortcut (for display), icon, callback (?)
viewButton : S.Address Model.Action -> Button -> Html
viewButton chan button =
    div
      [ class "actionbar-button"
      , onClick chan button.action
      ]
      [ text button.name ]

buttonGroupSep : Html
buttonGroupSep = div [ class "actionbar-vertsep" ] []
