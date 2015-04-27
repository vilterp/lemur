module Main where

import Shell

import Diagrams.Wiring as DW
import GraphEditor as GE

import Html exposing (..)
import Signal as S
import Actions

htmlUpdates = S.mailbox Actions.NoOp

mouseUpdates : Signal (DW.CollageLocation, DW.PrimMouseEvent)
mouseUpdates = DW.makeUpdateStream GE.editorLocFunc

updates = S.merge (S.map (GraphEditorUpdate << GE.MouseUpdate) mouseUpdates)
                  (htmlUpdates.signal)

state : Signal State
state = S.foldp Shell.update Shell.initState updates

main : Signal Html
main =
  S.map (Shell.view htmlUpdates.address) state
