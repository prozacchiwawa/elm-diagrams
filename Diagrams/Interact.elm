module Diagrams.Interact exposing
    ( InteractionState, MouseState
    , initInteractState, update, initMouseState
    , processMouseEvent, diagramOf, withDiagram
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

import Window
import Mouse

import List as L
import Maybe as M
import Element as E
import Collage as C

import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Actions exposing (..)

import Debug

{-|-}
type alias MouseState t a =
    { isDown : Bool
    , overPickedTags : List (PickedTag t a)
    , overPathsOnMouseDown : Maybe (List (List t))
    }

{-|-}
initMouseState : MouseState t a
initMouseState =
    { isDown = False
    , overPickedTags = []
    , overPathsOnMouseDown = Nothing
    }

{-|-}
type alias InteractionState t a =
    { mouseState : MouseState t a
    , diagram : Diagram t a
    }

{-| Function to update the interaction state, given an event (probably from `Diagrams.Wiring`'s `makeUpdateStream`)
-- the other top-level interface. -}
update : (CollageLocation, PrimMouseEvent) -> InteractionState t a -> (InteractionState t a, List a)
update (loc, evt) intState =
  let
    e2 =
      { ty = evt.ty
      , pt =
          ( ((Tuple.first evt.pt) * loc.dims.width) - loc.dims.width / 2.0
          , (((Tuple.second evt.pt) * loc.dims.height) - loc.dims.height / 2.0) * -1.0
          )
      }
    (newMS, actions) =
      processMouseEvent intState.diagram intState.mouseState e2
  in
  ({ intState | mouseState = newMS }, actions)

withDiagram : Diagram t a -> InteractionState t a -> InteractionState t a
withDiagram dia state = { state | diagram = dia }

diagramOf : InteractionState t a -> Diagram t a
diagramOf = .diagram

{-|-}
initInteractState : Diagram t a -> InteractionState t a
initInteractState diagram =
  { mouseState = initMouseState
  , diagram = diagram
  }

-- BUG: no initial pick path

-- TODO: fix these docs vv
{-| Given diagram with mouse state (`MouseDiagram`), mouse event, and dimensions of collage, return
new `MouseDiagram` with list of actions triggered by this mouse event. -}
processMouseEvent : Diagram t a -> MouseState t a -> PrimMouseEvent -> (MouseState t a, List a)
processMouseEvent diagram mouseState mouseEvt =
    let
      overTree = pick diagram (Debug.log "mouseEvt" mouseEvt).pt -- need to pick every time because actions may have changed
      overPickedTags = preorderTraverse overTree
      overPaths = tagPaths overPickedTags
      oldOverPickedTags = mouseState.overPickedTags
      oldOverPaths = tagPaths oldOverPickedTags
    in
      case mouseEvt.ty of
        
        MouseMoveEvt ->
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

        MouseDownEvt ->
          ( { mouseState | isDown = True
                         , overPathsOnMouseDown = Just overPaths
                         , overPickedTags = overPickedTags }
          , applyActions <| L.filterMap (getHandler .mouseDown) overPickedTags
          )

        MouseUpEvt ->
          let
            mouseUps =
              L.filterMap (getHandler .mouseUp) overPickedTags

            clicks =
              if overPaths == M.withDefault [] mouseState.overPathsOnMouseDown
              then L.filterMap (getHandler .click) overPickedTags
              else []
          in
            ( { mouseState | isDown = False
                           , overPathsOnMouseDown = Nothing
                           , overPickedTags = overPickedTags }
            , applyActions <| clicks ++ mouseUps
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
