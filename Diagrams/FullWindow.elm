module Diagrams.FullWindow exposing (..)

{-| Utilities for when you just want to get a diagram on the whole screen.

See `Diagrams.Wiring` docs for more info on `CollageLocation`s.

@docs view
-}

import Window
import Element as E
import Collage as C

import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)

{-|-}
view : (Int, Int) -> Diagram t a -> E.Element
view (w,h) d =
  C.collage w h [render d]
