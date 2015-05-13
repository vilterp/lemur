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
                  |> L.map runView
                  |> ul [ class "module-elements" ]
              )
          ]
      ]

modElementView : S.Address Model.Action -> (Model.FuncName, Model.Func) -> Html
modElementView addr (funcId, func) =
    li
      [ class "module-element"
      , onClick addr <| Model.AddApNode funcId 
      ]
      [ div [ class "element-icon" ] [ elementIcon func ]
      , div [ class "element-label" ] [ text <| Model.funcName func ]
      ]

-- TODO: click-to-open
runView : (Model.RunId, Model.Run) -> Html
runView (runId, run) =
    li
      -- TODO: this isn't a module element, but want same style. refactor CSS...
      [ class "module-element" ]
      [ text <| "#" ++ toString runId ++ ": " ++ run.userFunc.name ++ (if Model.runIsDone run then " (done)" else "") ]
