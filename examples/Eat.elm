module Eat exposing (main)

import Html.App exposing (program)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2)
import List exposing (map)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import Text exposing (fromString)
import AnimationFrame
import String
import Time exposing (Time)
import Keyboard.Extra as Keyboard


-- MODEL


type alias Model meta =
    { bodies : List (Body meta)
    , user : Body meta
    }


type alias Meta =
    Bool


{-| meta is used to tell if the body has been eaten
-}
meta =
    False


( width, height ) =
    ( 600, 600 )
bColor =
    yellow


someBodies =
    [ bubble bColor 20 1 e0 ( -80, 0 ) ( -1.5, 0 ) meta
    , bubble bColor 15 1 e0 ( 0, 200 ) ( -0.4, 1.0 ) meta
    , bubble bColor 5 1 e0 ( 200, -200 ) ( -1, -1 ) meta
    , bubble bColor 15 5 0.4 ( 100, 100 ) ( 1, 1 ) meta
    , bubble bColor 10 1 e0 ( 200, 200 ) ( 1, -1 ) meta
    , box bColor ( 10, 10 ) 1 e0 ( 200, 0 ) ( 0, 0 ) meta
    , box bColor ( 20, 20 ) 1 e0 ( -200, 0 ) ( 3, 0 ) meta
    , box bColor ( 15, 15 ) 1 e0 ( 200, -200 ) ( -1, -1 ) meta
    ]
        ++ bounds ( width - 50, height - 50 ) 100 e0 ( 0, 0 ) meta


user =
    bubble blue 100 1 e0 ( -80, 0 ) ( 0, 0 ) meta


model0 : Model Meta
model0 =
    { bodies = someBodies
    , user = user
    }



-- VIEW


scene : ( Model meta, Keyboard.Model ) -> Element
scene ( model, keyboard ) =
    collage width height <| map drawBody (model.user :: model.bodies)


drawBody : Body meta -> Form
drawBody { color, pos, velocity, inverseMass, restitution, shape, meta } =
    let
        veloLine =
            segment ( 0, 0 ) (mul2 velocity 5) |> traced (solid red)

        ready =
            case shape of
                Bubble radius ->
                    group
                        [ circle radius
                            |> filled color
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                        group
                            [ rect (w * 2) (h * 2) |> outlined (solid black)
                            ]
    in
        Collage.move pos ready



-- UPDATE


type Msg
    = Tick Time
    | KeyPress Keyboard.Msg


update : Msg -> ( Model meta, Keyboard.Model ) -> ( ( Model meta, Keyboard.Model ), Cmd Msg )
update msg ( model, keyboard ) =
    case msg of
        Tick dt ->
            let
                ( collidedUser, collidedBodies ) =
                    collideUser model.user model.bodies

                newUser =
                    (uncurry Engine.update (noGravity dt)) collidedUser
            in
                ( ( { model
                        | user = newUser
                        , bodies = (uncurry step (noGravity dt) collidedBodies)
                    }
                  , keyboard
                  )
                , Cmd.none
                )

        KeyPress keyMsg ->
            let
                ( kybrd, keyboardCmd ) =
                    Keyboard.update keyMsg keyboard

                direction =
                    Keyboard.arrows kybrd

                updatedUser =
                    Body.move model.user direction
            in
                ( ( { model
                        | user = updatedUser
                        , bodies = model.bodies
                    }
                  , keyboard
                  )
                , Cmd.map KeyPress keyboardCmd
                )


collideUser : Body meta -> List (Body meta) -> ( Body meta, List (Body meta) )
collideUser user bodies =
    List.foldl
        (\b ( u, bs ) ->
            let
                ( u2, b2 ) =
                    collide u b
            in
                ( u2, b2 :: bs )
        )
        ( user, [] )
        bodies


{-| collide assumes a0 is the user, b0 is possible food
-}
collide : Body meta -> Body meta -> ( Body meta, Body meta )
collide a0 b0 =
    let
        collisionResult =
            Engine.collision a0 b0

        ( a1, b1 ) =
            if collisionResult.penetration > 0 then
                case b0.shape of
                    Bubble r ->
                        if collisionResult.penetration > r * 2 || collisionResult.penetration < r then
                            ( a0, b0 )
                            -- swallow
                        else
                            (Engine.resolveCollision collisionResult a0 b0)

                    Box _ ->
                        Engine.resolveCollision collisionResult a0 b0
            else
                Engine.resolveCollision collisionResult a0 b0
    in
        ( a1, b1 )



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
            { init = ( ( model0, keyboard ), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }



-- HELPERS


inf =
    1 / 0


{-| default restitution coefficient
-}
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
