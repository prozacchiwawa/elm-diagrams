module Diagrams.Type exposing (..)

import Mouse
import Window

import Diagrams.RealType
import Diagrams.Geom exposing (..)

{-|
@docs Diagram
-}

type Msg
    = Mouse Mouse.Position
    | Down Mouse.Position
    | Up Mouse.Position
    | Size Window.Size

{-| A diagram. See `Diagrams.Core` for constructor functions. -}
type alias Diagram t a =
    Diagrams.RealType.Diagram t a

{-| Position of a rectangle on the screen in which a diagram will be drawn (as a `Graphics.Collage`).
Offset is difference between top left of screen and top left of collage, increasing right and down. -}
type alias CollageLocation =
  OffsetDimsBox

{-| Given window size, where on screen and how big is your collage? -}
type alias CollageLocFunc =
  Dims -> CollageLocation

