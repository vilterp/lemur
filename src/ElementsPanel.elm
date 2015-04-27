module ElementsPanel where

import Signal

import Model
import Shell.Common exposing (..)

-- MODEL

type alias ElemPanelState =
    { filterStr : Maybe String
    }

emptyElemPanelState = { filterStr = Nothing }

type Action
    = Filter String

type ElementType = Workflow | App | Datatype

-- CONTROLLER

update : Action -> State -> State
update action state =
    case action of
      Filter filter -> { state | filterStr <- if filter == "" then Nothing else Just filter }

-- VIEW

view : S.Address Model.Action -> Model.State -> Html
view chan state =
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

moduleSectionView : LC.LocalChannel .... -> SidebarModule -> Int -> Maybe Int -> Html
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

modElementView : LC.LocalChannel .... -> ModuleElement -> Bool -> Html
modElementView chan el pos selected =
    let className = "module-element" ++ (if selected then " selected" else "")
    in li [ class className, onClick (LC.send chan <| ClickOn pos) ] [ elementLabel el ]
