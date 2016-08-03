module Player exposing (main)

{-| # Overview
A basic example of using BoxesAndBubbles.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.

# Running

@docs main

-}

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


width =
    600


height =
    600


someBodies =
    [ bubble black 20 1 e0 ( -80, 100 ) ( 1.5, 0 ) defaultLabel
    -- , bubble 1 inf 0 ( 80, 0 ) ( 0, 0 ) defaultLabel
    , bubble black 15 1 e0 ( 0, 200 ) ( 0.4, -3.0 ) defaultLabel
    , bubble black 5 1 e0 ( 200, -280 ) ( -2, 1 ) defaultLabel
    , bubble black 15 5 0.4 ( 100, 100 ) ( -4, -3 ) defaultLabel
    , bubble black 10 1 e0 ( 200, 200 ) ( -5, -1 ) defaultLabel
    , box black ( 10, 10 ) 1 e0 ( 300, 0 ) ( 0, 0 ) defaultLabel
    , box black ( 20, 20 ) 1 e0 ( -200, 0 ) ( 3, 0 ) defaultLabel
    , box black ( 15, 15 ) 1 e0 ( 200, -200 ) ( -1, -1 ) defaultLabel
    ]
        ++ bounds ( width - 50, height - 50 ) 100 e0 ( 0, 0 ) defaultLabel


user =
    bubble black 100 1 e0 ( -80, 0 ) ( 0, 0 ) defaultLabel



-- we'll just compute the label from the data in the body


bodyLabel restitution inverseMass =
    [ "e = ", toString restitution, "\nm = ", toString (round (1 / inverseMass)) ] |> String.concat


type alias Labeled =
    { label : String }


type alias LabeledBody =
    Body Labeled



--attachlabel label body =
--  let labelRecord = { label = label }
--  in { body }
-- and attach it to all the bodies


labeledBodies : Model String
labeledBodies =
    map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies



-- why yes, it draws a body with label. Or creates the Element, rather


drawBody : Body String -> Form
drawBody { pos, velocity, inverseMass, restitution, shape, meta } =
    let
        veloLine =
            segment ( 0, 0 ) (mul2 velocity 5) |> traced (solid red)

        info =
            meta |> fromString |> centered |> toForm

        ready =
            case shape of
                Bubble radius ->
                    group
                        [ circle radius
                            -- |> filled blue
                            |>
                                outlined (solid black)
                        , info
                            |> Collage.move ( 0, radius + 16 )
                        , veloLine
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                        group
                            [ rect (w * 2) (h * 2) |> outlined (solid black)
                            , info |> Collage.move ( 0, h + 16 )
                            , veloLine
                            ]
    in
        Collage.move pos ready


scene : ( Body String, Model String, Keyboard.Model ) -> Element
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


collide : Body meta -> Body meta -> ( Body meta, Body meta )
collide a0 b0 =
    let
        collisionResult =
            Engine.collision a0 b0

        ( a1, b1 ) =
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
                    Body.move user direction
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
            { init = ( ( user, labeledBodies, keyboard ), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }
