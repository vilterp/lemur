module Main where

import Shell.Model (..)
import Shell.View (..)
import Shell.Controller (..)
import Shell.TestData (..)

import Diagrams.Wiring as DW

import Html (..)
import Signal as S

htmlUpdates = S.channel NoOp

-- TODO: scale with window
editorLocFunc : DW.CollageLocFunc
editorLocFunc windowDims =
  { offset = (252, 101), dims = { width = 800, height = 800 } }

mouseUpdates : Signal (DW.CollageLocation, DW.PrimMouseEvent)
mouseUpdates = DW.makeUpdateStream editorLocFunc

updates = S.merge (S.map MouseUpdate mouseUpdates) (S.subscribe htmlUpdates)

state : Signal State
state = S.foldp update initState updates

main : Signal Html
main =
  S.map (view htmlUpdates) state
