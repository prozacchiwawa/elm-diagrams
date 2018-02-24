module Diagrams.Interact exposing
    ( InteractionState
    , init, update, view, diagramOf, withDiagram
    )

{-| An abstraction for making diagrams which change as a function of the mouse.

Attach `ActionSet`s (see `Diagrams.Actions`) to diagrams with `Core.tagWithActions`;
then use `interactFold` or `update` to process mouse interaction. Mouse state (what 
is being clicked on, etc) is wrapped up inside an `InteractionState` value.

`updateModel` can be used to push in updates from external sources.

Look at GraphEditor for an example. (TODO: better docs / tutorial; explore using
Mailboxes to push out updates)

# Interaction
@docs InteractionState, MouseState, initInteractState
@docs initMouseState, interactFold, update
@docs processMouseEvent

-}

import Diagrams.Geom exposing (..)

import Json.Decode as JD
import Html exposing (Html, Attribute, div)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Mouse
import Window

import List as L
import Maybe as M
import Element as E
import Collage as C

import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.FullWindow exposing (..)

import Debug

{-|-}
type alias MouseState t a =
    { isDown : Bool
    , overPickedTags : List (PickedTag t a)
    , overPathsOnMouseDown : Maybe (List (List t))
    , windowSize : OffsetDimsBox
    }

{-|-}
initMouseState : Window.Size -> MouseState t a
initMouseState size =
    { isDown = False
    , overPickedTags = []
    , overPathsOnMouseDown = Nothing
    , windowSize =
          { offset = (0,0)
          , dims = { width = toFloat size.width, height = toFloat size.height }
          }
    }

{-|-}
type alias InteractionState t a =
    { mouseState : MouseState t a
    , diagram : Diagram t a
    }

{-| Function to update the interaction state, given an event (probably from `Diagrams.Wiring`'s `makeUpdateStream`)
-- the other top-level interface. -}
update : Msg -> InteractionState t a -> (InteractionState t a, List a)
update evt intState =
  let (newMS, actions) =
      processMouseEvent intState.diagram intState.mouseState evt
  in
  ({ intState | mouseState = newMS }, actions)
  
withDiagram : Diagram t a -> InteractionState t a -> InteractionState t a
withDiagram dia state = { state | diagram = dia }

diagramOf : InteractionState t a -> Diagram t a
diagramOf = .diagram

{-|-}
init : Diagram t a -> Window.Size -> InteractionState t a
init diagram size =
  { mouseState = initMouseState size
  , diagram = diagram
  }

-- BUG: no initial pick path
mouseLocation mouseState evt =
    let
        loc = mouseState.windowSize
        pos =
            case evt of
                Diagrams.Type.Mouse pos -> pos
                Diagrams.Type.Down pos -> pos
                Diagrams.Type.Up pos -> pos
                _ -> { x = 0, y = 0 }
    in
    ( (toFloat pos.x) - loc.dims.width / 2.0
    , ((toFloat pos.y) - loc.dims.height / 2.0) * -1.0
    )
    
-- TODO: fix these docs vv
{-| Given diagram with mouse state (`MouseDiagram`), mouse event, and dimensions of collage, return
new `MouseDiagram` with list of actions triggered by this mouse event. -}
processMouseEvent : Diagram t a -> MouseState t a -> Msg -> (MouseState t a, List a)
processMouseEvent diagram mouseState evt =
    let
      pt = mouseLocation mouseState evt
      overTree = pick diagram pt -- need to pick every time because actions may have changed
      overPickedTags = preorderTraverse overTree
      overPaths = tagPaths overPickedTags
      oldOverPickedTags = mouseState.overPickedTags
      oldOverPaths = tagPaths oldOverPickedTags
    in
    case evt of
        Diagrams.Type.Mouse _ ->
            let
                enters = L.filterMap (getHandler .mouseEnter) <|
                    L.filter (\pTag -> not <| L.member (tagPath pTag) oldOverPaths) overPickedTags

                leaves = L.filterMap (getHandler .mouseLeave) <|
                    L.filter (\pTag -> not <| L.member (tagPath pTag) overPaths) oldOverPickedTags

                moves = L.filterMap (getHandler .mouseMove) <|
                    L.filter (\pTag -> L.member (tagPath pTag) oldOverPaths) overPickedTags

            in
            ( { mouseState | overPickedTags = overPickedTags }
            , applyActions <| enters ++ leaves ++ moves
            )

        Diagrams.Type.Down _ ->
            ( { mouseState | isDown = True
              , overPathsOnMouseDown = Just overPaths
              , overPickedTags = overPickedTags }
            , applyActions <| L.filterMap (getHandler .mouseDown) overPickedTags
            )

        Diagrams.Type.Up _ ->
            let
                mouseUps =
                    L.filterMap (getHandler .mouseUp) overPickedTags
                        
                clicks =
                    if overPaths == M.withDefault [] mouseState.overPathsOnMouseDown
                    then L.filterMap (getHandler .click) overPickedTags
                    else []
            in
            ( { mouseState
              | isDown = False
              , overPathsOnMouseDown = Nothing
              , overPickedTags = overPickedTags
              }
            , applyActions <| clicks ++ mouseUps
            )

        Diagrams.Type.Size size ->
            ({ mouseState
             | windowSize =
                 { offset = (0,0)
                 , dims =
                       { width = toFloat size.width
                       , height = toFloat size.height
                       }
                 }
             }, []
          )

-- helpers for processMouseEvent

{-|-}
type alias PickedTag t a =
    { actionSet : ActionSet t a
    , offset : Point
    , tag : t
    , path : PickPath t
    }

{-|-}
tagPaths : List (PickedTag t a) -> List (List t)
tagPaths pTags =
    L.map tagPath pTags

{-|-}
tagPath : PickedTag t a -> List t
tagPath pTag = L.map .tag pTag.path

preorderTraverse : Maybe (PickTree t a) -> List (PickedTag t a)
preorderTraverse maybeTree =
    let
      recurse path tree =
        case tree of
          PickLeaf -> []
          PickTag {tag, offset, actionSet, child} ->
              let to = {tag=tag, offset=offset}
              in (recurse ({tag=tag, offset=offset}::path) child)
                    ++ [{ offset = offset, actionSet = actionSet, tag = tag, path = to::path }]
          PickLayers layers -> L.concatMap (recurse path) layers
    in
      case maybeTree of
        Just tree -> recurse [] tree
        Nothing -> []

getHandler : (ActionSet t a -> Maybe (EventToAction t a))
          -> PickedTag t a
          -> Maybe (PickedTag t a, EventToAction t a)
getHandler getMember pTag =
  case getMember pTag.actionSet of
    Just e2a -> Just (pTag, e2a)
    Nothing -> Nothing

applyActions : List (PickedTag t a, EventToAction t a) -> List a
applyActions pickedTags = 
  mapWithEarlyStop
    (\(pTag, e2a) -> e2a <| MouseEvent { offset = pTag.offset, path = pTag.path })
    pickedTags
  |> L.concat

{-| Like map, but stops if the second element of the function result is True. -}
mapWithEarlyStop : (a -> (b, Bool)) -> List a -> List b
mapWithEarlyStop f l =
  case l of
    [] -> []
    (x::xs) -> case f x of
                 (y, True) -> [y]
                 (y, False) -> y :: (mapWithEarlyStop f xs)

ptDecoder mt =
    JD.map2 Mouse.Position
        (JD.field "offsetX" JD.int)
        (JD.field "offsetY" JD.int)
    |> JD.map mt
               
view : List (Attribute Msg) -> InteractionState t a -> Html Msg
view attrs interaction =
    div attrs
        [ Diagrams.FullWindow.view
              ( round interaction.mouseState.windowSize.dims.width
              , round interaction.mouseState.windowSize.dims.height
              )
              (diagramOf interaction)
        |> E.toHtml
        , div
              [ on "mousemove" (ptDecoder Diagrams.Type.Mouse)
              , on "mousedown" (ptDecoder Diagrams.Type.Down)
              , on "mouseup"   (ptDecoder Diagrams.Type.Up)
              , style
                   [ ("position", "absolute")
                   , ("left", "0")
                   , ("top", "0")
                   , ("width", "100%")
                   , ("height", "100%")
                   , ("z-index", "2")
                   ]
              ]
              []
        ]
