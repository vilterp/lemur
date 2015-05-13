module ActionBar where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal as S
import List as L

import Model

import Debug

type alias State = List ButtonGroup
type alias ButtonGroup = List Button
type alias Button =
    { name : String
    , action : Action
    }
type Action
    = CodeAction
    | NormalAction Model.Action

-- TODO: move to model or some other module?
type alias CodeReq = Maybe (Model.Module, String)

getABState : Model.State -> State
getABState smState =
    [ [{ name = "Add Lambda", action = NormalAction Model.AddLambda }]
    , [{ name = "Run", action = CodeAction }]
    ]

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
            CodeAction ->
                onClick codeAddr <| Just (state.mod, state.editingFn)
            NormalAction action ->
                onClick actionAddr action
    in div
        [ class "actionbar-button"
        , actionAttr
        ]
        [ text button.name ]

buttonGroupSep : Html
buttonGroupSep = div [ class "actionbar-vertsep" ] []
