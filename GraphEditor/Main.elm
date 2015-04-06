module GraphEditor.Main where

import Signal
import Window

import Diagrams.Core (Diagram)
import Diagrams.Wiring as DW
import Diagrams.FullWindow as DFW
import Diagrams.Interact as DI

import GraphEditor.Model (..)
import GraphEditor.TestData (initState)
import GraphEditor.Controller as Cont

-- DATA

-- start 'er up

diagrams : Signal (Diagram Tag Action)
diagrams = DI.interactFold Cont.update Cont.render DFW.fullWindowCollageLocFunc initState

main = Signal.map2 DFW.fullWindowView Window.dimensions diagrams
