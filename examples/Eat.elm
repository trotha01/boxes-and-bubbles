module Example exposing (main)

{-| # Overview
A basic example of using BoxesAndBubbles.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.

# Running

@docs main

-}

import Html.App exposing (program)
import BoxesAndBubbles.Bodies as Bodies exposing (..)
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

inf =
    1 / 0



-- infinity, hell yeah


e0 =
    0.8



-- default restitution coefficient
-- box: (w,h) pos velocity density restitution
-- bubble: radius pos velocity density restitution


type alias Model meta =
    List (Body meta)


defaultLabel =
    ""

{- meta is used to tell if the body has been eaten -}
meta = False


width =
    600


height =
    600

bColor = yellow

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

-- why yes, it draws a body with label. Or creates the Element, rather


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


scene : ( Body meta, Model meta, Keyboard.Model ) -> Element
scene ( user, bodies, keyboard ) =
    collage width height <| map drawBody (user :: bodies)



-- different force functions to experiment with


noGravity t = ( ( 0, 0.0 ), ( 0, 0 ) )

constgravity t =
     ( ( 0, -0.2 ), ( 0, 0 ) )



-- constant downward gravity


sinforce t =
    ( (sin <| radians (t / 1000)) * 50, 0 )



-- sinusoidal sideways force


counterforces t =
    ( ( 0, -0.01 ), ( 0, t / 1000 ) )



-- small gravity, slowly accellerating upward drift


type Msg
    = Tick Time
    | KeyPress Keyboard.Msg


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , AnimationFrame.diffs Tick
        ]

{-| collide assumes a0 is the user, b0 is possible food
-}
collide : Body meta -> Body meta -> ( Body meta, Body meta )
collide a0 b0 =
    let
        collisionResult =
            Engine.collision a0 b0

        (a1, b1) = if collisionResult.penetration > 0
            then case b0.shape of
                (Bubble r) -> if collisionResult.penetration > r*2 || collisionResult.penetration < r
                    then (a0, b0) -- swallow
                    else (Engine.resolveCollision collisionResult a0 b0)
                (Box _) -> Engine.resolveCollision collisionResult a0 b0
            else
                Engine.resolveCollision collisionResult a0 b0
    in
        ( a1, b1 )


collideUser : Body meta -> Model meta -> ( Body meta, Model meta )
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

update : Msg -> ( Body meta, Model meta, Keyboard.Model ) -> ( ( Body meta, Model meta, Keyboard.Model ), Cmd Msg )
update msg ( user, bodies, keyboard ) =
    case msg of
        Tick dt ->
            let
                ( collidedUser, collidedBodies ) =
                    collideUser user bodies

                newUser =
                    (uncurry Engine.update (noGravity dt)) collidedUser
            in
                ( ( newUser, (uncurry step (noGravity dt) collidedBodies), keyboard ), Cmd.none )

        KeyPress keyMsg ->
            let
                ( kybrd, keyboardCmd ) =
                    Keyboard.update keyMsg keyboard

                direction =
                    Keyboard.arrows kybrd

                updatedUser =
                    Bodies.move user direction
            in
                ( ( updatedUser, bodies, keyboard ), Cmd.map KeyPress keyboardCmd )


{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}
main : Program Never
main =
    let
        ( keyboard, keyboardCmd ) =
            Keyboard.init
    in
        program
            { init = ( ( user, someBodies, keyboard ), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }