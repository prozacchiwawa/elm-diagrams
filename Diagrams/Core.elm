module Diagrams.Core exposing
    ( circle, rect, path, polygon, text, spacer, transform
    , group, tag, tagWithActions, ngon, eqTriangle
    , move, moveX, moveY, scale, rotate
    , render
    , empty, vspace, hspace, vline, hline
    )

{-| Diagrams is a library built on top of `Graphics.Collage` which allows you to
construct graphics by laying out elements relative to each other.

A Diagram is represented as a tree of elements, where the leaves are primitive
shapes like rectangles, circles, paths and text and the nodes are transformations
like translation, rotation, and scaling.

There are also `Group` nodes. These have multiple children which are transformed
simultaneously by the transformations above them.

[Sierpinski triangle example](https://gist.github.com/vilterp/9966fd18de8d9b282ade)

Lastly, there are `Tag` nodes which just hold a child Diagram t and a value of type a;
these exist solely to identify a subdiagram, for the purposes of (a) specifying a tag
path and getting the coordinates it was positioned at (the `getCoords` function) or
(b) given a point, find what subtree it is over (the `pick` function).

Using signals to compose `pick` with mouse clicks, you can create a signal of
clicked-on elements. Folding this with the application state and re-rendering, you
can make an interface which is responsive to the mouse without channels.

The library is based on the excellent [Diagrams][hd] library for Haskell, which
has a nice [visual tutorial][hd-tut]. Things are named slightly differently, and this
version is missing a lot of features and generality.

With v5.0 the functionality has been split into many modules to make it more manageable;
this [graph of their dependencies][mod-graph] may be helpful. Not sure if a `Prelude` module
which exports everything is a good idea.

 [hd]: http://projects.haskell.org/diagrams/
 [hd-tut]: http://projects.haskell.org/diagrams/doc/quickstart.html
 [mod-graph]: https://docs.google.com/drawings/d/1_321XRPhfP8t0u747QhNwR_PiibVHroxcioLq-vHdq8/edit

# Constructors
@docs circle, rect, path, polygon, text, spacer, transform, group, tag, tagWithActions, ngon, eqTriangle

# Basic Transforms
@docs move, moveX, moveY, scale, rotate

# Rendering
@docs render

# Composition Utilities
@docs empty, vspace, hspace, vline, hline

-}

import Collage as C
import Element as E
import Text as T
import List as L
import Transform
import Color

import Diagrams.Geom exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.RealType exposing (..)
import Diagrams.Type as Type

-- constructors

-- TODO: shouldn't fill style come first in these? I dunno

{-| Circle with a given radius and fill, centered on the local origin. -}
circle : Float -> FillStroke -> Type.Diagram t a
circle = Circle

{-| Rectangle with given width, height, and fill, centered on the local origin. -}
rect : Float -> Float -> FillStroke -> Type.Diagram t a
rect = Rect

{-| Unclosed path made of this list of points, laid out relative to the local origin. -}
path : List Point -> C.LineStyle -> Type.Diagram t a
path points ls =
  Path points ls

{-|-}
polygon : List Point -> FillStroke -> Type.Diagram t a
polygon points fs =
  Polygon points fs

{-| Text with given style, centered vertically and horizontally on the local origin. -}
text : T.Style -> String -> Type.Diagram t a
text style txt =
  let
    te =
      T.fromString txt |> T.style style |> E.centered
  in
    Text txt style te

{-| Spacer with given width and height; renders as transparent. -}
spacer : Float -> Float -> Type.Diagram t a
spacer w h = rect w h invisible

{-| Translate, rotate, or scale a given diagram. The transformed diagram has the
same origin. -}
transform : Transform -> Type.Diagram t a -> Type.Diagram t a
transform = TransformD

{-| Group a list of Diagrams in to one. Elements will be stacked with local origins
on top of one another. This is the same as `zcat`. The first diagram in the list is on top. -}
group : List (Type.Diagram t a) -> Type.Diagram t a
group = Group

{-| Return a Tag node with the given Diagram as its sole child. Adding this to the 
diagram tree is useful for picking and getting coordinates. -}
tag : t -> Type.Diagram t a -> Type.Diagram t a
tag t dia = Tag t emptyActionSet dia

{-| Return a Tag node with the given Diagram as its sole child, holding both
a tag and an action set. -}
tagWithActions : t -> ActionSet t a -> Type.Diagram t a -> Type.Diagram t a
tagWithActions = Tag

{-| equilateral triangle with given side length & fill/stroke style -}
eqTriangle : Float -> FillStroke -> Type.Diagram t a
eqTriangle sideLength fs = ngon 3 sideLength fs

-- adapted from Graphics.Collage
{-| regular polygon with number of sides, side length, & fill/stroke style -}
ngon : Int -> Float -> FillStroke -> Type.Diagram t a
ngon n r fs =
  let m = toFloat n
      t = 2 * pi / m
      f i = ( r * cos ((t*i) + pi/2), r * sin ((t*i) + pi/2) )
  in polygon (L.map (toFloat >> f) (List.range 0 (round (m-1)))) fs

-- basic transformations

{-| Rotate the given diagram counterclockwise. Angle specified in radians. -}
rotate : Float -> Type.Diagram t a -> Type.Diagram t a
rotate r d = TransformD (Rotate r) d

{-| Translate given diagram by (x, y). Origin of resulting diagram is the same. -}
move : (Float, Float) -> Type.Diagram t a -> Type.Diagram t a
move (x, y) dia = TransformD (Translate x y) dia

{-|-}
moveX : Float -> Type.Diagram t a -> Type.Diagram t a
moveX x = move (x, 0)

{-|-}
moveY : Float -> Type.Diagram t a -> Type.Diagram t a
moveY y = move (0, y)

{-|-}
scale : Float -> Type.Diagram t a -> Type.Diagram t a
scale s d = TransformD (Scale s) d

-- rendering

{-|-}
render : Type.Diagram t a -> C.Form
render d =
  let
    handleFS fs shape =
      let filled =
            case fs.fill of
              Just fillStyle ->
                case fillStyle of
                  Solid color -> [C.filled color shape]
                  Texture src -> [C.textured src shape]
                  Grad grad -> [C.gradient grad shape]
              Nothing -> []
          stroked =
            case fs.stroke of
              Just strokeStyle ->
                [C.outlined strokeStyle shape]
              Nothing -> []
      in C.group <| stroked ++ filled
 in
  case d of
    Tag _ _ dia ->
      render dia
    Group dias ->
      C.group <| L.map render <| L.reverse dias -- TODO: this seems semantically right; don't want to
                                                            -- have to reverse tho
    TransformD (Scale s) dia ->
      C.scale s <| render dia
    TransformD (Rotate r) dia ->
      C.rotate r <| render dia
    TransformD (Translate x y) dia ->
      C.move (x, y) <| render dia
    Text str ts te ->
      C.text <| T.style ts <| T.fromString str
    Path path ls ->
      C.path path |> C.traced ls
    Polygon points fs ->
      handleFS fs <| C.polygon points
    Rect w h fs ->
      handleFS fs <| C.rect w h
    Circle r fs ->
      handleFS fs <| C.circle r

-- shortcuts

{-|-}
empty : Type.Diagram t a
empty = spacer 0 0

{-| Vertical spacer of height h -}
vspace : Float -> Type.Diagram t a
vspace h = spacer 0 h

{-| Horizontal spacer of width w -}
hspace : Float -> Type.Diagram t a
hspace w = spacer w 0

{-| Vertical line of given height and line style -}
vline : Float -> C.LineStyle -> Type.Diagram t a
vline h ls = path [(0, h/2), (0, -h/2)] ls

{-| Horizontal line of given width and line style -}
hline : Float -> C.LineStyle -> Type.Diagram t a
hline w ls = path [(-w/2, 0), (w/2, 0)] ls
