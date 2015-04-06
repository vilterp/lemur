module GraphEditor.Styles where

import Text as T
import Graphics.Collage as C
import Color

import GraphEditor.Model (..)

defaultTextStyle = T.defaultStyle
titleStyle = { defaultTextStyle | bold <- True }
slotLabelStyle = defaultTextStyle

defaultLineStyle = C.defaultLine

edgeStyle = { defaultLineStyle | width <- 3 }

defLine = C.defaultLine

nodeTopDivider = defLine
nodeMiddleDivider = { defLine | dashing <- [5, 5] }

normalPortColor = Color.yellow

portStateColorCode : PortState -> Color.Color
portStateColorCode st = case st of
                          ValidPort -> Color.lightGreen
                          InvalidPort -> Color.grey
                          NormalPort -> normalPortColor
                          TakenPort -> normalPortColor

lambdaNodeBgColor = Color.RGBA 220 255 255 1 -- cyan
apNodeBgColor = Color.lightBlue
ifNodeBgColor = Color.lightPurple
