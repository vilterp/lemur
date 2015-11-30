module CommonView where

import Graphics.Collage as C
import Html exposing (..)
import Html.Attributes exposing (..)
import Text exposing (defaultStyle)
import Color
import String

import Diagrams.Core as DC
import Diagrams.FillStroke as DFS
import Diagrams.Align as DA
import Diagrams.Envelope as DE
import Diagrams.Geom as DG
import Diagrams.Pad as DP

import Model
import Runtime.Model

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

icon : Color.Color -> Color.Color -> Char -> DC.Diagram t a
icon bgColor letterColor letter =
    (DC.text { defaultStyle | color = letterColor } (String.fromChar letter))
      `DA.atop` (DC.circle 10 <| DFS.justFill <| DFS.Solid bgColor)

udfIcon = icon Color.lightBlue Color.black 'G' -- graph
pythonIcon = icon Color.yellow Color.black 'P' -- python
runIcon = icon Color.brown Color.white 'R'

elementIcon : Model.Func -> Html
elementIcon func =
    let curIcon = case func of
                    Model.GraphFunc _ -> udfIcon
                    Model.PythonFunc _ -> pythonIcon
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

runLabel : (Model.RunId, Model.Run) -> String
runLabel (runId, run) =
    "#" ++ toString runId ++ ": "
      ++ run.graphFuncName
      ++ (if Runtime.Model.isDone run.callTree then " (done)" else "")

-- tooltip
-- maybe this should go in a diagrams-contrib library or something

type alias TooltipSettings =
    { direction : DG.Direction
    , background : DFS.FillStroke
    , tipSpacing : Float
    , diaSpacing : Float
    }

defaultTooltipSettings =
    { direction = DG.Down
    , background = DFS.justFill <| DFS.Solid Color.black
    , tipSpacing = 5
    , diaSpacing = 3
    }

tooltip : TooltipSettings -> DC.Diagram t a -> DC.Diagram t a
tooltip settings dia =
    let mainBox = DP.background settings.background (dia |> DP.pad settings.diaSpacing)
        triangle = DC.eqTriangle 7 settings.background
                      |> DC.rotate (DG.directionAngle settings.direction - pi/2)
    in case settings.direction of
      DG.Up    -> triangle `DA.above`  mainBox  |> DA.alignTop    |> DC.moveY -settings.tipSpacing
      DG.Down  -> mainBox  `DA.above`  triangle |> DA.alignBottom |> DC.moveY settings.tipSpacing
      DG.Left  -> triangle `DA.beside` mainBox  |> DA.alignLeft   |> DC.moveX settings.tipSpacing
      DG.Right -> mainBox  `DA.beside` triangle |> DA.alignRight  |> DC.moveX -settings.tipSpacing
