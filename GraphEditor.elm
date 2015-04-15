module GraphEditor where

import Html
import Graphics.Collage as C

import Diagrams.Interact as DI
import Diagrams.Geom as DG
import Diagrams.Core as D

import GraphEditor.Model as GEM
import GraphEditor.View as GEV
import GraphEditor.Controller as GEC

type alias State = DI.InteractionState GEM.State GEM.Tag GEM.Action

initState : GEM.State -> State
initState state = DI.initInteractState GEV.viewGraph state

-- (CollageLocation, PrimMouseEvent) -> InteractionState m t a -> InteractionState m t a
update : DI.InteractUpdateFunc GEM.State GEM.Tag GEM.Action
update = DI.makeFoldUpdate GEC.update GEV.viewGraph

view : State -> DG.Dims -> Html.Html
view state dims =
    Html.fromElement <| C.collage (round dims.width) (round dims.height) <| [D.render state.diagram]
