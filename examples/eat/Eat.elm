module Eat exposing (main)

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


type alias Meta =
    Bool


{-| eaten is the meta in Body meta is used to tell if the body has been eaten
-}
eaten =
    False


width =
    600


height =
    600


bColor =
    yellow


someBodies =
    [ bubble bColor 20 1 e0 ( -80, 0 ) ( -1.5, 0 ) eaten
    , bubble bColor 15 1 e0 ( 0, 200 ) ( -0.4, 1.0 ) eaten
    , bubble bColor 5 1 e0 ( 200, -200 ) ( -1, -1 ) eaten
    , bubble bColor 15 5 0.4 ( 100, 100 ) ( 1, 1 ) eaten
    , bubble bColor 10 1 e0 ( 200, 200 ) ( 1, -1 ) eaten
    , box bColor ( 10, 10 ) 1 e0 ( 200, 0 ) ( 0, 0 ) eaten
    , box bColor ( 20, 20 ) 1 e0 ( -200, 0 ) ( 3, 0 ) eaten
    , box bColor ( 15, 15 ) 1 e0 ( 200, -200 ) ( -1, -1 ) eaten
    ]
        ++ bounds ( width - 50, height - 50 ) 100 e0 ( 0, 0 ) eaten


initUser =
    bubble blue 100 1 e0 ( -80, 0 ) ( 0, 0 ) eaten


model0 : Model Bool
model0 =
    { bodies = someBodies
    , user = initUser
    , keys = []
    }



-- VIEW


scene : Model Bool -> Collage msg
scene model =
    group <| map drawBody (model.bodies ++ [ model.user ])


drawBody : Body meta -> Collage msg
drawBody { color, pos, velocity, inverseMass, restitution, shape, meta } =
    let
        veloLine =
            segment ( 0, 0 ) (mul2 velocity 5) |> traced (solid 1 (uniform red))

        ready =
            case shape of
                Bubble radius ->
                    group
                        [ circle radius
                            |> filled (uniform color)
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                    group
                        [ rectangle (w * 2) (h * 2) |> outlined (solid 1 (uniform black))
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
                            Engine.resolveCollision collisionResult a0 b0

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
        , onAnimationFrameDelta Tick
        ]



-- MAIN


type alias Flags =
    {}


init : Flags -> ( Model Bool, Cmd Msg )
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
