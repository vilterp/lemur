module Main where

import Shell.Model exposing (..)
import Shell.View exposing (..)
import Shell.Controller exposing (..)
import Shell.TestData exposing (..)

import Diagrams.Wiring as DW
import GraphEditor as GE

import Html exposing (..)
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
