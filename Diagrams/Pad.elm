module Diagrams.Pad exposing (..)

{-| Surround diagrams with padding.

@docs padSpecific, pad, background, outlineBox
-}

-- TODO: maybe make this a little more like the CSS box model (border, padding, margin)
-- and give it a name evocative of that

import Collage as C

import Diagrams.Core exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Envelope exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Type exposing (..)

-- TODO: better name...?
{-| Given four numbers up, down, left, and right, put an invisible spacer
behind the given diagram, changing its envelope. -}
padSpecific : Float -> Float -> Float -> Float -> Diagram t a -> Diagram t a
padSpecific u d l r dia =
    let bbox = boundingBox dia
        paddedBbox = { up = bbox.up + u
                     , down = bbox.down + d
                     , left = bbox.left + l
                     , right = bbox.right + r }
        offsetDims = bbox2offsetDims paddedBbox
        padder = move (offsetDims.offset) <| spacer (offsetDims.dims.width) (offsetDims.dims.height)
    in zcat [dia, padder]

{-| Return given diagram surrounded by given padding on all sides. -}
pad : Float -> Diagram t a -> Diagram t a
pad pd dia = padSpecific pd pd pd pd dia

{-| Put a rectangle behind the given diagram, matching its bounding box. -}
background : FillStroke -> Diagram t a -> Diagram t a
background fs dia = let offsetDims = bbox2offsetDims <| boundingBox dia
                        bg = move offsetDims.offset <| rect offsetDims.dims.width offsetDims.dims.height fs
                    in zcat [dia, bg]

{-| Draw a box around the given diagram -}
outlineBox : C.LineStyle -> Diagram t a -> Diagram t a
outlineBox ls dia = background (justStroke ls) <| pad (ls.width/2) dia
