module GraphEditor.Main where

import Signal
import Window

import Diagrams.Core exposing (Diagram)
import Diagrams.Wiring as DW
import Diagrams.FullWindow as DFW
import Diagrams.Interact as DI

import GraphEditor.Model as GEM
import TestData
import GraphEditor.Controller as GEC
import GraphEditor.View as GEV

-- DATA

-- start 'er up

initState = 
    { mod = TestData.helloMap
    , funcName = "main"
    , dragState = Nothing
    , pan = (0, 0)
    }

diagrams : Signal (Diagram GEM.Tag GEM.Action)
diagrams = DI.interactFold GEC.update GEV.viewGraph DFW.fullWindowCollageLocFunc initState

main = Signal.map2 DFW.fullWindowView Window.dimensions diagrams
