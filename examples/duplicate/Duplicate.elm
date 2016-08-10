module Duplicate exposing (main)

{-| # Overview
Duplicate the user after a time period
-}

import Html.App exposing (program)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, plus)
import List exposing (map)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Text
import Random
import User exposing (init)
import Bodies
import Wall
import Bound


-- MODEL


type alias Model =
    { bodies : List (Body Bodies.Meta)
    , user : User.Model
    , children : List (Body Bodies.Meta)
    , walls : Wall.Model Wall.Meta
    , bounds : Bound.Model Bound.Meta
    , seed : Random.Seed
    , points : Int
    , keyboard : Keyboard.Model
    }


initialModel : Keyboard.Model -> Model
initialModel keyboard =
    { bodies = Bodies.init
    , user = User.init
    , children = []
    , walls = Wall.init width
    , bounds = Bound.init width height
    , seed = Random.initialSeed 3
    , points = 0
    , keyboard = keyboard
    }



-- VIEW


( height, width ) =
    ( 700, 700 )
( halfHeight, halfWidth ) =
    ( height / 2, width / 2 )
scene : Model -> Element
scene model =
    collage width height
        <| ((User.view model.user)
                :: (Bodies.view model.bodies ++ Bodies.view model.children)
                ++ (Wall.view model.walls)
           )
        ++ points model


points : Model -> List Form
points model =
    [ (text (Text.fromString (toString model.points))) |> Collage.move ( halfWidth - 50, halfHeight - 50 ) ]



-- UPDATE


type Msg
    = Tick Time
    | Points Int
    | KeyPress Keyboard.Msg
    | BoundMsg (Bound.Msg Bodies.Meta)
    | Regenerate (Body Bodies.Meta)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Points p ->
            let
                ( ( _, children, _ ), _ ) =
                    User.update User.MakeChild ( model.user, model.keyboard )

                model2 =
                    { model | points = model.points + p }

                model3 =
                    if model2.points /= 0 && model2.points % 100 == 0 then
                        { model2 | children = model2.children ++ children }
                    else
                        model2
            in
                ( model3, Cmd.none )

        Tick dt ->
            let
                -- update user
                ( ( user1, children, _ ), _ ) =
                    User.update (User.Tick dt) ( model.user, model.keyboard )

                -- collide user with the bodies
                ( user2, bodies2 ) =
                    User.collideWithBodies user1 model.bodies

                -- collide user with the children
                -- ( user3, children2) =
                --     User.collideWithBodies user1 (model.children ++ children)
                -- collide user with the wall
                user4 =
                    Wall.collideWith model.walls user2

                -- update the body collisions
                ( bodies3, pointMsgs ) =
                    Bodies.update (Bodies.Tick dt) (bodies2)

                -- update the children collisions
                ( children3, _ ) =
                    Bodies.update (Bodies.Tick dt) (model.children ++ children)

                -- collide children with the bounds
                ( children4, _ ) =
                    Bound.collideWithBodies model.bounds children3

                -- collide bodies with the bounds
                ( bodies4, msgs' ) =
                    Bound.collideWithBodies model.bounds bodies3

                model2 =
                    { model | user = user4, bodies = bodies4, children = children4 }

                -- hack, since I don't know how to generate a Cmd
                ( model3, cmd2 ) =
                    List.foldl (\msg ( m, cmd ) -> (update (BoundMsg msg) m))
                        ( model2, Cmd.none )
                        msgs'

                -- hack2, since I don't know how to generate a Cmd
                ( model4, cmd3 ) =
                    List.foldl
                        (\msg ( m, cmd ) ->
                            case msg of
                                Bodies.Points p ->
                                    (update (Points p) m)

                                _ ->
                                    ( m, cmd )
                        )
                        ( model3, Cmd.none )
                        pointMsgs
            in
                ( model4, cmd3 )

        KeyPress keyMsg ->
            let
                ( ( updatedUser, _, keyboard ), keyboardCmd ) =
                    User.update (User.KeyPress keyMsg) ( model.user, model.keyboard )
            in
                ( { model | user = updatedUser, keyboard = keyboard }, Cmd.map KeyPress keyboardCmd )

        Regenerate body ->
            let
                ( newBody, newSeed ) =
                    Bodies.regenerate model.seed body

                newModel =
                    { model | bodies = model.bodies ++ [ newBody ], seed = newSeed }
            in
                ( newModel, Cmd.none )

        BoundMsg msg ->
            case msg of
                Bound.Regenerate body ->
                    update (Regenerate body) model

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , AnimationFrame.diffs Tick
        ]



-- MAIN


main : Program Never
main =
    let
        ( keyboard, keyboardCmd ) =
            Keyboard.init
    in
        program
            { init = ( (initialModel keyboard), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }



-- HELPERS


inf : Float
inf =
    1 / 0


{-| e0 default restitution coefficient
-}
e0 : Float
e0 =
    0.8



-- different force functions to experiment with


noGravity t =
    ( ( 0, 0.0 ), ( 0, 0 ) )


{-| constant downward gravity
-}
constgravity t =
    ( ( 0, -0.2 ), ( 0, 0 ) )


{-| sinusoidal sideways force
-}
sinforce t =
    ( (sin <| radians (t / 1000)) * 50, 0 )


{-| small gravity, slowly accellerating upward drift
-}
counterforces t =
    ( ( 0, -0.01 ), ( 0, t / 1000 ) )
