module Player exposing (main)

import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles.Math2D exposing (mul2)
import Browser exposing (..)
import Browser.Events exposing (..)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (..)
import Collage.Text exposing (fromString)
import Color exposing (..)
import Keyboard
import Keyboard.Arrows as Keyboard
import List exposing (map)
import String



-- MODEL


type alias Model meta =
    { bodies : List (Body meta)
    , user : Body meta
    , keys : List Keyboard.Key
    }


defaultLabel =
    ""


width =
    600


height =
    600


{-| someBodies

  - box: (w,h) pos velocity density restitution
  - bubble: radius pos velocity density restitution

-}
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


initUser =
    bubble black 100 1 e0 ( -80, 0 ) ( 0, 0 ) defaultLabel


model0 : Model String
model0 =
    { bodies = map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass }) someBodies
    , user = initUser
    , keys = []
    }


bodyLabel restitution inverseMass =
    [ "e = ", String.fromFloat restitution, "\nm = ", String.fromInt (round (1 / inverseMass)) ] |> String.concat



-- VIEW


scene : Model String -> Collage msg
scene model =
    group <| map drawBody (model.user :: model.bodies)


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



-- UPDATE


type Msg
    = Tick Float
    | KeyPress Keyboard.Msg


update : Msg -> Model meta -> ( Model meta, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( collidedUser, collidedBodies ) =
                    collideUser model.user model.bodies

                ( gravity, ambiant ) =
                    noGravity dt

                newUser =
                    Engine.update gravity ambiant collidedUser
            in
            ( { model
                | user = newUser
                , bodies = step gravity ambiant collidedBodies
              }
            , Cmd.none
            )

        KeyPress keyMsg ->
            let
                pressedKeys =
                    Keyboard.update keyMsg model.keys

                direction =
                    Keyboard.arrows pressedKeys

                updatedUser =
                    Body.move model.user direction
            in
            ( { model
                | user = updatedUser
                , bodies = model.bodies
                , keys = pressedKeys
              }
            , Cmd.none
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


collide : Body meta -> Body meta -> ( Body meta, Body meta )
collide a0 b0 =
    let
        collisionResult =
            Engine.collision a0 b0

        ( a1, b1 ) =
            Engine.resolveCollision collisionResult a0 b0
    in
    ( a1, b1 )



-- SUBSCRIPTIONS


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , onAnimationFrameDelta Tick
        ]



-- MAIN


type alias Flags =
    {}


init : Flags -> ( Model String, Cmd Msg )
init flags =
    ( model0, Cmd.none )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always subs
        , view = scene >> Collage.Render.svgBox ( width, height )
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
