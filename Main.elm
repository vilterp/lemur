module Main where

import Shell.Model (..)
import Shell.View (..)
import Shell.Controller (..)
import Shell.TestData (..)

import Html (..)
import Signal as S

updates = S.channel NoOp

state : Signal State
state = S.foldp update initState (S.subscribe updates)

main : Signal Html
main =
  S.map (view updates) state
