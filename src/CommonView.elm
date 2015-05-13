module CommonView where

import Graphics.Collage as C
import Html exposing (..)
import Html.Attributes exposing (..)
import Text exposing (defaultStyle)
import Color

import Diagrams.Core as DC
import Diagrams.FillStroke as DFS
import Diagrams.Align as DA
import Diagrams.Envelope as DE

import Model

panel : List Html -> List Html -> Html
panel header_elems child_elems =
    div
      [ class "panel" ]
      [ div [ class "panel-header" ] header_elems
      , div [ class "panel-contents" ] child_elems
      ]

scrollPanel : List Html -> List Html -> Html
scrollPanel header_elems child_elems =
    panel
        header_elems
        [ div
            [ class "panel-scroll" ]
            [ div
                [ class "panel-scroll-contents" ]
                child_elems 
            ]
        ]

elementIcon : Model.Func -> Html
elementIcon func =
    let icon color letter =
          (DC.text letter defaultStyle)
            `DA.atop` (DC.circle 10 <| DFS.justFill <| DFS.Solid color)
        curIcon = case func of
                    Model.UserFunc _ -> icon Color.lightBlue "U"
                    Model.BuiltinFunc _ -> icon Color.yellow "B"
    in curIcon |> asHtml

asHtml : DC.Diagram t a -> Html.Html
asHtml dia =
    [DC.render dia]
      |> C.collage (round <| DE.width dia) (round <| DE.height dia)
      |> Html.fromElement

panelSection : String -> Html -> Html
panelSection name contents =
    div
      [ class "panel-contents-section" ]
      [ div
          [ class "panel-contents-heading" ]
          [ div [ class "heading-label" ] [ text name ]
          --, div [ class "heading-button" ] [ text "+" ]
          ]
      , div
          [ class "panel-contents-list" ]
          [ contents
          ]
      ]
