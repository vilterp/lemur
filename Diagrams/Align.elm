module Diagrams.Align where

{-|
# Relative Positioning
@docs beside, above, atop, hcat, vcat, zcat

# Alignment
@docs alignLeft, alignRight, alignTop, alignBottom, alignCenter, HAlign, VAlign, hAlign, vAlign, hcatA, vcatA
-}

import List as L

import Diagrams.Core (..)
import Diagrams.Geom (..)
import Diagrams.Envelope (..)

-- Relative Positioning

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
beside : Diagram t a -> Diagram t a -> Diagram t a
beside a b = let xTrans = (envelope Right a) + (envelope Left b)
             in Group [a, TransformD (Translate xTrans 0) b]

{-| Given two diagrams a and b, place b to the right of a, such that their origins
are on a horizontal line and their envelopes touch. The origin of the new diagram
is the origin of a. -}
above : Diagram t a -> Diagram t a -> Diagram t a
above a b = let yTrans = (envelope Down a) + (envelope Up b)
            in Group [a, TransformD (Translate 0 -yTrans) b]

{-| Given two diagrams a and b, stack a on top of b in the "out of page" axis,
so a occludes b. -}
atop : Diagram t a -> Diagram t a -> Diagram t a
atop a b = Group [a, b]

{-| Place a list of Diagrams next to each other, such that
their origins are along a horizontal line. The first element in the list will
be on the left; the last on the right. -}
hcat : List (Diagram t a) -> Diagram t a
hcat = L.foldr beside empty

{-| Place a list of Diagrams next to each other, such that
their origins are along a vertical line. The first element in the list will
be on the top; the last on the bottom. -}
vcat : List (Diagram t a) -> Diagram t a
vcat = L.foldr above empty

{-| Place a list of diagrams on top of each other, with their origin points
stacked on the "out of page" axis. The first diagram in the list is on top.
This is the same as the `group`. -}
zcat : List (Diagram t a) -> Diagram t a
zcat = Group -- lol

-- Alignment

{-| Translate a diagram such that the origin is on the left edge of the bounding box -}
alignLeft : Diagram t a -> Diagram t a
alignLeft dia = let leftEnv = envelope Left dia
                in moveX leftEnv dia

{-| Translate a diagram such that the origin is on the right edge of the bounding box -}
alignRight : Diagram t a -> Diagram t a
alignRight dia = let rightEnv = envelope Right dia
                 in moveX -rightEnv dia


{-| Translate a diagram such that the origin is on the top edge of the bounding box -}
alignTop : Diagram t a -> Diagram t a
alignTop dia = let upEnv = envelope Up dia
               in moveY -upEnv dia

{-| Translate a diagram such that the origin is on the bottom edge of the bounding box -}
alignBottom : Diagram t a -> Diagram t a
alignBottom dia = let downEnv = envelope Down dia
                  in moveY downEnv dia

{-| Translate a diagram such that the envelope in all directions is equal -}
alignCenter : Diagram t a -> Diagram t a
alignCenter dia = let left = envelope Left dia
                      right = envelope Right dia
                      xTrans = (right - left)/2
                      up = envelope Up dia
                      down = envelope Down dia
                      yTrans = (down-up)/2
                  in move (-xTrans, yTrans) dia

type HAlign = LeftA | RightA
type VAlign = TopA | BottomA

hAlign : HAlign -> Diagram t a -> Diagram t a
hAlign ha dia = case ha of
                  LeftA -> alignLeft dia
                  RightA -> alignRight dia

vAlign : VAlign -> Diagram t a -> Diagram t a
vAlign va dia = case va of
                  TopA -> alignTop dia
                  BottomA -> alignBottom dia

hcatA : VAlign -> List (Diagram t a) -> Diagram t a
hcatA va dias = hcat <| L.map (vAlign va) dias

vcatA : HAlign -> List (Diagram t a) -> Diagram t a
vcatA ha dias = vcat <| L.map (hAlign ha) dias
