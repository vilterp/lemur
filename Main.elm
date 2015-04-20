module Main where

import Shell.Model (..)
import Shell.View (..)
import Shell.Controller (..)
import Shell.TestData (..)

import Diagrams.Wiring as DW
import GraphEditor as GE

import Html (..)
import Signal as S

htmlUpdates = S.channel NoOp

mouseUpdates : Signal (DW.CollageLocation, DW.PrimMouseEvent)
mouseUpdates = DW.makeUpdateStream GE.editorLocFunc

updates = S.merge (S.map (GraphEditorUpdate << GE.MouseUpdate) mouseUpdates)
                  (S.subscribe htmlUpdates)

state : Signal State
state = S.foldp update initState updates

main : Signal Html
main =
  S.map (view htmlUpdates) state
