module Shell.View where

import List as L
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import LocalChannel as LC
import Signal as S

import GraphEditor as GE
import Shell.Model (..)

view : (S.Channel Update) -> State -> Html
view updates state =
    let sidebarUpdates = LC.create ElemPanelUpdate updates
    in div
        [ id "app" ]
        [ topSection
        , actionBar
        , elementsPanelView sidebarUpdates state.sidebarState
        , centerSection state
        , rightSection
        ]

-- TODO: probably split each section into its own module with model, view, and controller

-- TOP SECTION

topSection : Html
topSection =
    div
      [ id "top" ]
      [ div [ id "logo" ] [ text "Swift/V" ]
      , div [ class "breadcrumbs" ]
          [ div [ class "breadcrumb" ] [ text "EasySIM" ]
          , div [ class "breadcrumb breadcrumb-sep" ] [ text ">" ]
          , div [ class "breadcrumb" ]
              [ elementLabel { typ = Workflow, name = "Demo" } ]
          ]
      , div [ id "user-state" ]
          [ div [ id "prof-pic" ] [ text "PV" ]
          , div [ id "username" ] [ text "vilterp" ]
          , div [ id "logout" ] [ text "(Logout)" ]
          ]
      ]

-- ACTION BAR

actionBar : Html
actionBar =
    div
      [ id "action-bar" ]
      [ actionbarButton "Undo"
      , actionbarButton "Redo"
      , actionbarSep
      , actionbarButton "Back"
      , actionbarButton "Forward"
      , actionbarSep
      , actionbarButton "Insert Literal"
      , actionbarSep
      , actionbarButton "Factor out"
      , actionbarButton "Rename"
      , actionbarButton "Open Definition"
      , actionbarButton "Visualize"
      , actionbarSep
      , actionbarButton "Run"
      ]

-- Later: KB shortcut (for display), icon, callback (?)
actionbarButton : String -> Html
actionbarButton name = div [ class "actionbar-button" ] [ text name ]

actionbarSep : Html
actionbarSep = div [ class "actionbar-vertsep" ] []

-- CENTER SECTION

centerSection : State -> Html
centerSection state =
    div
      [ id "center" ]
      [ div
          [ class "panel-header tabs" ]
          [ tab { typ = Workflow, name = "Demo" } True
          , tab { typ = App, name = "DSSAT" } False
          , tab { typ = Datatype, name = "PSims Weather" } False
          ]
      , div
          [ class "center-scroll" ]
          [ div
              [ id "canvas-viewport"
              , style [("width", "100%"), ("height", "100%"), ("position", "absolute"), ("overflow", "hidden"), ("background", "white url(/img/light_gray_grid.gif) repeat;")]
              ]
              [GE.view state.graphState state.editorLoc.dims]
          ]
      ]

tab : ModuleElement -> Bool -> Html
tab elem selected =
  let className = if selected then "tab selected" else "tab"
  in div [ class className ] [ elementLabel elem ]

-- RIGHT SECTION

rightSection : Html
rightSection =
    div
      [ id "right" ]
      [ scrollPanel
          [ div [ class "panel-header-label" ] [ text "Editor" ] ]
          [ p [] [ text lipsum1 ]
          , p [] [ text lipsum2 ] 
          ]
      ]

-- LEFT SIDEBAR

-- TODO: make naming consistent. it's the elements panel, not the sidebar.
-- TODO: move to own file


elementsPanelView : LC.LocalChannel SidebarAction -> SidebarState -> Html
elementsPanelView chan state =
    div [ id "left" ]
      [ scrollPanel
          [ div [ class "panel-header-label" ] [ text "Elements" ]
          , input [ type' "text", id "search-bar" ] []
          ]
          (let indices = [0..L.length state.modules]
               selected idx = case state.selection of
                                Nothing -> Nothing
                                Just sel ->
                                    case sel of
                                      ElemSelection {modIdx, elemIdx} ->
                                        if modIdx == idx
                                        then Just elemIdx
                                        else Nothing
               selections = L.map selected indices
           in L.map3 (moduleSectionView chan) state.modules indices selections)
      ]

moduleSectionView : LC.LocalChannel SidebarAction -> SidebarModule -> Int -> Maybe Int -> Html
moduleSectionView chan {name, elements} modIdx selection =
    div
      [ class "panel-contents-section" ]
      [ div
          [ class "panel-contents-heading" ]
          [ div [ class "heading-label" ] [ text name ]
          , div [ class "heading-button" ] [ text "+" ]
          ]
      , div
          [ class "panel-contents-list" ]
          [ ul [ class "module-elements" ]
              (let indices = [0..L.length elements]
                   positions = L.map (\idx -> { modIdx = modIdx, elemIdx = idx }) indices
                   checkSelected idx = case selection of
                                         Nothing -> False
                                         Just selIdx -> selIdx == idx
                   selected = L.map checkSelected indices
               in L.map3 (modElementView chan) elements positions selected)
          ]
      ]

modElementView : LC.LocalChannel SidebarAction -> ModuleElement -> SidebarElemPos -> Bool -> Html
modElementView chan el pos selected =
    let className = "module-element" ++ (if selected then " selected" else "")
    in li [ class className, onClick (LC.send chan <| ClickOn pos) ] [ elementLabel el ]

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

-- TODO: can just do these with Elm graphics instead of SVG!

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

lipsum1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laboru"
lipsum2 = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?"
