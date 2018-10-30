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
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Keyboard
import List exposing (map)
import String
import Task


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always subs
        , view = view
        }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel, Task.perform ViewportChange getViewport )



-- MODEL


type alias Model =
    { bodies : List (Body String)
    , keys : List Keyboard.Key
    , viewport : Viewport
    }


initModel : Model
initModel =
    { bodies = []
    , keys = []
    , viewport = initViewport 0 0
    }


initViewport : Int -> Int -> Viewport
initViewport width height =
    { scene =
        { height = 0
        , width = 0
        }
    , viewport =
        { height = toFloat height
        , width = toFloat width
        , x = 0
        , y = 0
        }
    }


defaultLabel =
    ""


someBodies model =
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
        ++ bounds ( model.viewport.viewport.width - 50, model.viewport.viewport.height - 50 ) 100 e0 ( 0, 0 ) defaultLabel


bodyLabel restitution inverseMass =
    [ "e = ", String.fromFloat restitution, "\nm = ", String.fromInt (round (1 / inverseMass)) ] |> String.concat


type alias Labeled =
    { label : String }


type alias LabeledBody =
    Body Labeled


initBodies : Model -> List (Body String)
initBodies model =
    map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) (someBodies model)



-- UPDATE


type Msg
    = Tick Float
    | KeyPress Keyboard.Msg
    | ViewportChange Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
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

        ViewportChange viewport ->
            { model | viewport = viewport }
                |> (\newModel -> { newModel | bodies = initBodies newModel })
                |> (\finalModel -> ( finalModel, Cmd.none ))



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ Collage.Render.svgBox ( model.viewport.viewport.width, model.viewport.viewport.height ) <| scene model
        ]


scene : Model -> Collage msg
scene model =
    group <| map drawBody model.bodies


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



-- SUBS


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , onResize (\h w -> ViewportChange (initViewport h w))
        , onAnimationFrameDelta Tick
        ]



-- HELPERS


inf =
    1 / 0


e0 =
    0.8



-- different force functions to experiment with


noGravity t =
    ( ( 0, 0.0 ), ( 0, 0 ) )


constgravity t =
    -- constant downward gravity
    ( ( 0, -0.2 ), ( 0, 0 ) )


sinforce t =
    -- sinusoidal sideways force
    ( (sin <| radians (t / 1000)) * 50, 0 )


counterforces t =
    -- small gravity, slowly accellerating upward drift
    ( ( 0, -0.01 ), ( 0, t / 1000 ) )
