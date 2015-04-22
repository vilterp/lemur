module Shell.ActionBar where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal as S
import List as L

import Shell.Model as SM
import GraphEditor as GE

type alias State = List ButtonGroup
type alias ButtonGroup = List Button
type alias Button =
    { name : String
    , action : SM.Update
    }

getABState : SM.State -> State
getABState smState =
    [ [{ name = "Add Lambda", action = SM.GraphEditorUpdate GE.AddLambda }]
    ]

view : S.Address SM.Update -> SM.State -> Html
view chan state =
    getABState state
      |> L.map (viewButtonGroup chan)
      |> L.intersperse buttonGroupSep
      |> div [ id "action-bar" ]

viewButtonGroup : S.Address SM.Update -> ButtonGroup -> Html
viewButtonGroup chan bg =
    L.map (viewButton chan) bg
      |> div [ class "action-bar-button-group" ]

-- Later: KB shortcut (for display), icon, callback (?)
viewButton : S.Address SM.Update -> Button -> Html
viewButton chan button =
    div
      [ class "actionbar-button"
      , onClick chan button.action ]
      [ text button.name ]

buttonGroupSep : Html
buttonGroupSep = div [ class "actionbar-vertsep" ] []
