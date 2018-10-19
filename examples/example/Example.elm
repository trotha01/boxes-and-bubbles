module Example exposing (main)

{-|


# Overview

A basic example of using BoxesAndBubbles.
The drawing is supplied by this module (the BoxesAndBubbles library provides only the model).
The scene is updated after each animation frame.


# Running

@docs main

-}

import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles.Math2D exposing (Vec2, mul2)
import Browser exposing (..)
import Browser.Events exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Keyboard
import List exposing (map)
import String


inf =
    1 / 0



-- infinity, hell yeah


e0 =
    0.8



-- default restitution coefficient
-- box: (w,h) pos velocity density restitution
-- bubble: radius pos velocity density restitution


type alias Model meta =
    { bodies : List (Body meta)
    , keys : List Keyboard.Key
    }


initModel : Model String
initModel =
    { bodies = labeledBodies
    , keys = []
    }


defaultLabel =
    ""


width =
    600


height =
    600


someBodies =
    [ bubble black 20 1 e0 ( -80, 0 ) ( 1.5, 0 ) defaultLabel
    , bubble black 1 inf 0 ( 80, 0 ) ( 0, 0 ) defaultLabel
    , bubble black 15 1 e0 ( 0, 200 ) ( 0.4, -3.0 ) defaultLabel
    , bubble black 5 1 e0 ( 200, -280 ) ( -2, 1 ) defaultLabel
    , bubble black 15 5 0.4 ( 100, 100 ) ( -4, -3 ) defaultLabel
    , bubble black 10 1 e0 ( 200, 200 ) ( -5, -1 ) defaultLabel
    , box black ( 10, 10 ) 1 e0 ( 300, 0 ) ( 0, 0 ) defaultLabel
    , box black ( 20, 20 ) 1 e0 ( -200, 0 ) ( 3, 0 ) defaultLabel
    , box black ( 15, 15 ) 1 e0 ( 200, -200 ) ( -1, -1 ) defaultLabel
    ]
        ++ bounds ( width - 50, height - 50 ) 100 e0 ( 0, 0 ) defaultLabel



-- we'll just compute the label from the data in the body


bodyLabel restitution inverseMass =
    [ "e = ", String.fromFloat restitution, "\nm = ", String.fromInt (round (1 / inverseMass)) ] |> String.concat


type alias Labeled =
    { label : String }


type alias LabeledBody =
    Body Labeled



--attachlabel label body =
--  let labelRecord = { label = label }
--  in { body }
-- and attach it to all the bodies


labeledBodies : List (Body String)
labeledBodies =
    map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies



-- why yes, it draws a body with label. Or creates the Element, rather


drawBody : Body String -> Collage msg
drawBody { pos, velocity, inverseMass, restitution, shape, meta } =
    let
        veloLine =
            segment ( 0, 0 ) (mul2 velocity 5) |> traced (solid 1 (uniform red))

        info =
            meta |> fromString |> rendered |> Collage.Layout.center

        ready =
            case shape of
                Bubble radius ->
                    group
                        [ circle radius
                            -- |> filled blue
                            |> outlined (solid 1 (uniform black))
                        , info
                            |> Collage.shift ( 0, radius + 16 )
                        , veloLine
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                    group
                        [ rectangle (w * 2) (h * 2) |> outlined (solid 1 (uniform black))
                        , info |> Collage.shift ( 0, h + 16 )
                        , veloLine
                        ]
    in
    Collage.shift pos ready


scene : Model String -> Collage msg
scene model =
    -- collage width height <| map drawBody bodies
    group <| map drawBody model.bodies



-- different force functions to experiment with


noGravity t =
    ( ( 0, 0.0 ), ( 0, 0 ) )


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
    = Tick Float
    | KeyPress Keyboard.Msg


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , onAnimationFrameDelta Tick
        ]


update : Msg -> Model meta -> ( Model meta, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( gravity, ambiant ) =
                    constgravity dt
            in
            ( { model | bodies = step gravity ambiant model.bodies }, Cmd.none )

        KeyPress key ->
            let
                pressedKeys =
                    Keyboard.update key model.keys
            in
            ( { model | keys = pressedKeys }, Cmd.none )


{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}
type alias Flags =
    {}


init : Flags -> ( Model String, Cmd Msg )
init flags =
    ( initModel, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always subs
        , view = Collage.Render.svgBox ( width, height ) << scene
        }
