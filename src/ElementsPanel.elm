module ElementsPanel where

import Signal as S
import Dict as D
import List as L
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import CommonView exposing (..)

import Model

-- VIEW

view : S.Address Model.Action -> Model.State -> Html
view chan state =
    div [ id "left" ]
      [ scrollPanel
          [ div [ class "panel-header-label" ] [ text "Elements" ]
          --, input [ type' "text", id "search-bar" ] [] -- TODO search bar
          ]
          [ div
              [ class "panel-contents-section" ]
              [ div
                [ class "panel-contents-list" ]
                (
                  (state.mod.userFuncs |> D.toList)
                    ++ (state.mod.builtinFuncs |> D.toList)
                    |> L.map (modElementView chan)
                )
              ]
          ]
      ]

modElementView : S.Address Model.Action -> (Model.FuncName, Model.Func) -> Html
modElementView chan (funcId, func) =
    li
      [ class "module-element"
      , onClick chan <| Model.AddApNode funcId 
      ]
      [ div [ class "element-icon" ] [ elementIcon func ]
      , div [ class "element-label" ] [ text <| Model.funcName func ]
      ]
