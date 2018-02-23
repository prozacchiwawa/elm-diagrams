module Diagrams.FillStroke exposing (..)

{-| Construct fills and strokes for diagrams.

# Types
@docs FillStroke, FillStyle

# Constructors
@docs justFill, justStroke, fillAndStroke, invisible, justSolidFill, withAlpha, defaultStroke, halfStrokeWidth
-}

import Color
import Collage as C

{-| literally a copy of Graphics.Collage.FillStyle, cuz it's no longer exported -}
type FillStyle
    = Solid Color.Color
    | Texture String
    | Grad Color.Gradient

{-|-}
type alias FillStroke =
    { fill : Maybe FillStyle
    , stroke : Maybe C.LineStyle
    }

{-|-}
justFill : FillStyle -> FillStroke
justFill fs =
  { fill = Just fs, stroke = Nothing }

{-|-}
justStroke : C.LineStyle -> FillStroke
justStroke ls =
  { fill = Nothing, stroke = Just ls }

{-|-}
fillAndStroke : FillStyle -> C.LineStyle -> FillStroke
fillAndStroke fs ls =
  { fill = Just fs, stroke = Just ls }

{-|-}
invisible : FillStroke
invisible =
  { fill = Nothing, stroke = Nothing }

{-|-}
defaultStroke : C.LineStyle
defaultStroke =
  let defLine = C.defaultLine
  in { defLine | width = 3
               , cap = C.Padded }

{-|-}
justSolidFill : Color.Color -> FillStroke
justSolidFill color =
  justFill <| Solid color

{-|-}
halfStrokeWidth : FillStroke -> Float
halfStrokeWidth fs =
  case fs.stroke of
    Just ls -> ls.width/2
    Nothing -> 0

{-|-}
withAlpha : Float -> Color.Color -> Color.Color
withAlpha newAlpha color =
  let {red, green, blue, alpha} = Color.toRgb color
  in Color.rgba red green blue newAlpha
