module ElementsPanel where

import Signal as S
import Dict as D
import List as L
import Json.Decode as JsDec
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import CommonView exposing (..)

import Model

-- VIEW

view : S.Address Model.Action -> Model.State -> Html
view addr state =
    div [ id "left" ]
      [ scrollPanel
          [ div [ class "panel-header-label" ] [ text "Elements" ]
          --, input [ type' "text", id "search-bar" ] [] -- TODO search bar
          ]
          [ panelSection "Members" (
                (state.mod.userFuncs |> D.toList)
                      ++ (state.mod.builtinFuncs |> D.toList)
                      |> L.map (modElementView addr)
                      |> ul [ class "module-elements" ]
              )
          , panelSection "Runs" (
                state.runs
                  |> D.toList
                  |> L.map (runView addr)
                  |> ul [ class "module-elements" ]
              )
          ]
      ]

modElementView : S.Address Model.Action -> (Model.FuncName, Model.Func) -> Html
modElementView addr (funcId, func) =
    li
      [ class "module-element"
      , onClick addr <| Model.GraphAction <| Model.AddApNode funcId 
      ]
      [ div [ class "element-icon" ] [ elementIcon func ]
      , span [ class "element-label" ] [ text <| Model.funcName func ]
      , span
          [ class "edit-button"
          , let
              action =
                case func of
                  Model.UserFunc attrs -> Model.OpenUDF funcId
                  Model.BuiltinFunc attrs -> Model.OpenBuiltin funcId
            in
              onWithOptions
                "click"
                { stopPropagation = True, preventDefault = True }
                (JsDec.succeed ())
                (\_ -> S.message addr action)
          ]
          [ text "edit" ]
      ]

-- TODO: click-to-open
runView : S.Address Model.Action -> (Model.RunId, Model.Run) -> Html
runView addr runInfo =
    li
      -- TODO: this isn't a module element, but want same style. refactor CSS...
      [ class "module-element"
      , onClick addr <| Model.OpenRun <| fst runInfo
      ]
      [ div [ class "element-icon" ] [ runIcon |> asHtml ]
      , div [ class "element-label" ] [ runLabel runInfo |> text ]
      ]
