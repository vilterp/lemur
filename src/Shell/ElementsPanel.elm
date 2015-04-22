module Shell.ElementsPanel where

import Shell.Module (..)
import Shell.Common (..)

-- MODEL

type alias SidebarState =
    { modules : List Module
    , selection : Maybe Selection
    }

emptySidebarState = { modules = [], selection = Nothing }

type Selection = ElemSelection ElemPos

type alias ElemPos = { modIdx : Int, elemIdx : Int }

-- TODO: okay, but how do we forward events *up*??

type Action
    = InsertElem (ModName, Func)
    | ClickOn ElemPos
    | Down
    | Up
    | Left
    | Filter String
    | NoOp

type ElementType = Workflow | App | Datatype

-- CONTROLLER

update : Action -> State -> State
update action state =
    case (Debug.log "action" action) of
      ClickOn pos -> { state | selection <- Just <| ElemSelection pos }
      -- TODO
      Down -> state
      Up -> state
      Left -> state
      Filter str -> state
      SidebarNoOp -> state

-- VIEW

view : LC.LocalChannel SidebarAction -> SidebarState -> Html
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
