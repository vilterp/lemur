module CommonUI where

import Html (..)
import Html.Attributes (..)

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

elementLabel : ModuleElement -> Html
elementLabel {typ, name} =
    let imageName = case typ of
                      Workflow -> "workflow"
                      App -> "app"
                      Datatype -> "datatype"
        imgPath = "img/" ++ imageName ++ ".svg"
    in div [ class "element-label" ]
         [ img [ class "element-icon", src imgPath ] []
         , div [ class "element-label-text" ] [ text name ]
         ]
