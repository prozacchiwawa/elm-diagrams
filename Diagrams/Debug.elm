module Diagrams.Debug exposing (..)

{-| Utilities for debugging alignment issues, etc.

@docs showBBox, showOrigin
-}

import Color
import Collage as C

import Diagrams.Core exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Type exposing (..)

{-| Draw a red dot at `(0, 0)` in the diagram's local vector space. -}
showOrigin : Diagram t a -> Diagram t a
showOrigin d = let originPoint = circle 3 <| justFill <| Solid Color.red
               in atop originPoint d

{-| Draw a red dot box around a diagram. -}
showBBox : Diagram t a -> Diagram t a
showBBox d = let dfl = C.defaultLine
                 style = { dfl | width = 2
                               , color = Color.red }
             in outlineBox style d
