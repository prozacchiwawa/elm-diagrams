module Basic exposing (..)

import Element as E
import Collage as C
import Json.Decode as JD
import Mouse
import Window
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (on)

import Color
import Debug
import List as L
import Text as T
import Task exposing (Task)

-- whoooo all the moduless
import Diagrams.Core exposing (..)
import Diagrams.Type exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Interact exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)

type Tag = RectOrange
         | RectBlue
         | Circ
         | Textt

type Action
    = ClickCirc Point
    | EnterOrange Point
    | LeaveOrange Point
    | MoveBlue Point

type Msg
    = ExternalMsg Diagrams.Type.Msg
    | ActionMsg Action

defLine = C.defaultLine

testDia : Diagram Tag Action
testDia = let aPath = path [(-50,-50), (30, 100)] C.defaultLine
              rectOrange = tagWithActions RectOrange
                              { emptyActionSet | mouseEnter = Just <| keepBubbling <| (\(MouseEvent evt) -> [EnterOrange evt.offset])
                                               , mouseLeave = Just <| keepBubbling <| (\(MouseEvent evt) -> [LeaveOrange evt.offset]) }
                              <| rect 50 70 (fillAndStroke (Solid Color.orange) { defLine | width = 20, cap = C.Padded })
              rectBlue = tagWithActions RectBlue
                              { emptyActionSet | mouseMove = Just <| keepBubbling <| (\(MouseEvent evt) -> [MoveBlue evt.offset]) }
                              <| rect 70 50 (justSolidFill Color.blue)
              rects = vcat [ rectOrange , rectBlue ]
              circ = tagWithActions Circ
                            { emptyActionSet | click = Just <| keepBubbling <| (\(MouseEvent evt) -> [ClickCirc evt.offset]) }
                            <| circle 20 (fillAndStroke (Solid Color.yellow) { defLine | width = 2, cap = C.Padded })
              justText = text (let ds = T.defaultStyle in {ds | bold = True}) "Foo"
              someText = tag Textt <| background (justSolidFill Color.lightBlue) <| pad 5 <| justText
              stuff = atop circ (above rectOrange (beside rectBlue (above circ someText)))
              moreStuff = hcat <| L.intersperse circ (L.repeat 5 rectOrange)
          in showOrigin <| showBBox <| alignCenter <| (moreStuff |> above stuff |> above stuff)

type Model = Model
    { interact : InteractionState Tag Action }

init : (Model, Cmd Msg)
init =
    (Model
        { interact = Diagrams.Interact.init testDia { width = 100, height = 100 }
        }
    ) ! [Window.size |> Task.perform (Diagrams.Type.Size >> ExternalMsg)]

update msg (Model model) =
    let
        intmsg =
            case Debug.log "msg" msg of
                ExternalMsg m -> Just m
                ActionMsg m -> Nothing
    
        (applyToState, actions) =
            intmsg
            |> Maybe.map
               (\msg ->
                    let (i,a) = Diagrams.Interact.update msg model.interact in
                    (Model { model | interact = i }, a)
               )
            |> Maybe.withDefault (Model model, [])
    in
    List.foldr
        (\ev (model, eff) ->
            let (upd, nef) = update (ActionMsg ev) model in
            upd ! [eff, nef]
        )
        (applyToState ! [])
        actions

ptDecoder mt =
    JD.map2 Mouse.Position
        (JD.field "clientX" JD.int)
        (JD.field "clientY" JD.int)
    |> JD.map (ExternalMsg << mt)
               
view (Model model) =
    div [ ]
        [ Diagrams.Interact.view model.interact
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

subs _ =
    Sub.batch [ Window.resizes Size ]
    |> Sub.map ExternalMsg
       
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        }
