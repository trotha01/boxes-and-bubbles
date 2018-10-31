module Duplicate exposing (main)

{-|


# Overview

Duplicate the user after a time period

-}

import Bodies
import Bound
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles.Math2D exposing (mul2)
import Browser exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Keyboard
import Keyboard.Arrows as Keyboard
import List exposing (map)
import Random
import String
import Task
import User exposing (init)
import Wall



-- MODEL


type alias Model =
    { bodies : Bodies.Model
    , user : User.Model
    , children : Bodies.Model
    , walls : Wall.Model
    , bounds : Bound.Model
    , seed : Random.Seed
    , points : Int
    , keyboard : List Keyboard.Key
    , windowWidth : Int
    , windowHeight : Int
    }


initialModel : Model
initialModel =
    { bodies = Bodies.init
    , user = User.init
    , children = []
    , walls = Wall.init width height
    , bounds = Bound.init width height
    , seed = Random.initialSeed 3
    , points = 0
    , keyboard = []
    , windowWidth = 700
    , windowHeight = 700
    }



-- VIEW


height =
    700


width =
    700


scene : Model -> Collage msg
scene model =
    -- collage model.windowWidth model.windowHeight <|
    group <|
        Bodies.view model.bodies
            ++ Bodies.view model.children
            ++ Wall.view model.walls
            ++ points model
            ++ [ User.view model.user ]


points : Model -> List (Collage msg)
points model =
    let
        halfWidth =
            toFloat model.windowWidth / 2

        halfHeight =
            toFloat model.windowHeight / 2

        pointText =
            Collage.Text.fromString (String.fromInt model.points)
                |> rendered
    in
    [ pointText |> Collage.shift ( halfWidth - 50, halfHeight - 50 ) ]



-- UPDATE


type Msg
    = Tick Float
    | Points Int
    | KeyPress Keyboard.Msg
    | BoundMsg (Bound.Msg Bodies.Meta)
    | Regenerate (Body Bodies.Meta)
    | WindowResize Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowResize w h ->
            let
                walls =
                    Wall.update (Wall.Resize ( w, h )) model.walls

                bounds =
                    Bound.update (Bound.Resize ( w, h )) model.bounds
            in
            ( { model
                | windowWidth = w
                , windowHeight = h
                , walls = walls
                , bounds = bounds
              }
            , Cmd.none
            )

        Points p ->
            let
                newPoints =
                    model.points + p

                model2 =
                    if newPoints /= 0 && modBy 100 newPoints == 0 then
                        { model
                            | children = User.childFromModel model.user :: model.children
                            , points = newPoints
                        }

                    else
                        { model | points = newPoints }
            in
            ( model2, Cmd.none )

        Tick dt ->
            let
                -- update user
                ( ( user1, _ ), _ ) =
                    User.update (User.Tick dt) ( model.user, model.keyboard )

                -- collide user with the bodies
                ( user2, bodies2 ) =
                    User.collideWithBodies user1 model.bodies

                -- collide user with the children
                -- ( user3, children2) =
                --     User.collideWithBodies user1 model.children
                -- collide user with the wall
                user4 =
                    Wall.collideWith model.walls user2

                -- update the body collisions
                ( bodies3, pointMsgs ) =
                    Bodies.update (Bodies.Tick dt) bodies2

                -- update the children collisions
                ( children3, _ ) =
                    Bodies.update (Bodies.Tick dt) model.children

                -- collide children with the bounds
                ( children4, _ ) =
                    Bound.collideWithBodies model.bounds children3

                -- collide bodies with the bounds
                ( bodies4, msgs2 ) =
                    Bound.collideWithBodies model.bounds bodies3

                model2 =
                    { model | user = user4, bodies = bodies4, children = children4 }

                -- hack, since I don't know how to generate a Cmd
                -- we do the bound msgs here
                ( model3, cmd2 ) =
                    List.foldl (\msg2 ( m, cmd ) -> update (BoundMsg msg2) m)
                        ( model2, Cmd.none )
                        msgs2

                -- hack2, since I don't know how to generate a Cmd
                -- we do the point msgs here
                ( model4, cmd3 ) =
                    List.foldl
                        (\pointMsg ( m, cmd ) ->
                            case pointMsg of
                                Bodies.Points p ->
                                    update (Points p) m

                                _ ->
                                    ( m, cmd )
                        )
                        ( model3, Cmd.none )
                        pointMsgs
            in
            ( model4, cmd3 )

        KeyPress keyMsg ->
            let
                ( ( updatedUser, keyboard ), keyboardCmd ) =
                    User.update (User.KeyPress keyMsg) ( model.user, model.keyboard )
            in
            ( { model | user = updatedUser, keyboard = keyboard }, Cmd.map KeyPress keyboardCmd )

        Regenerate body ->
            let
                ( newBody, newSeed ) =
                    Bodies.regenerate model.seed body

                model2 =
                    { model | bodies = newBody :: model.bodies, seed = newSeed }
            in
            ( model2, Cmd.none )

        BoundMsg boundMsg ->
            case boundMsg of
                Bound.Regenerate body ->
                    update (Regenerate body) model

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , onAnimationFrameDelta Tick
        , Browser.Events.onResize WindowResize
        ]



-- MAIN


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always subs
        , view = scene >> Collage.Render.svgBox ( width, height )
        }



-- HELPERS


initialWindowSize : Cmd Msg
initialWindowSize =
    Task.perform
        (\vp ->
            WindowResize (round vp.viewport.width) (round vp.viewport.height)
        )
        Browser.Dom.getViewport
