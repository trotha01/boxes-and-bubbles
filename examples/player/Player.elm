module Player exposing (main)

import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles.Math2D as Vec2 exposing (getX, getY, mul2, vec2)
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
import Json.Decode as Decode exposing (Decoder)
import Keyboard
import Keyboard.Arrows as Keyboard
import List exposing (map)
import String
import Task



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always subs
        , view = view

        -- scene >> Collage.Render.svgBox ( width, height )
        }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel, Task.perform ViewportChange getViewport )



-- MODEL


type alias Model =
    { bodies : List (Body String)
    , user : Body String
    , keys : List Keyboard.Key
    , viewport : Viewport
    }


initModel : Model
initModel =
    { bodies = []
    , user = initUser
    , keys = []
    , viewport = initViewport 0 0
    }


initBodies : Model -> List (Body String)
initBodies model =
    someBodies model
        |> map (\b -> { b | meta = bodyLabel b.restitution b.inverseMass })


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


initUser =
    bubble black 100 1 e0 ( -80, 0 ) ( 0, 0 ) defaultLabel


defaultLabel =
    ""


someBodies model =
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
        ++ bounds ( model.viewport.viewport.width - 50, model.viewport.viewport.height - 50 ) 100 e0 ( 0, 0 ) defaultLabel


bodyLabel restitution inverseMass =
    [ "e = ", String.fromFloat restitution, "\nm = ", String.fromInt (round (1 / inverseMass)) ] |> String.concat



-- UPDATE


type Msg
    = Tick Float
    | KeyPress Keyboard.Msg
    | ViewportChange Viewport
    | Click Point


mapUser : (Body String -> Body String) -> Model -> Model
mapUser f model =
    { model | user = f model.user }


{-| since collage uses points relative to the center
we have to convert points from Browser, like onclick
-}
pointRelativeToCenter : Viewport -> Point -> Point
pointRelativeToCenter viewport point =
    { x = point.x - (viewport.viewport.width / 2)
    , y = 0 - (point.y - (viewport.viewport.height / 2))
    }


moveUser : Viewport -> Point -> Body String -> Body String
moveUser viewport point user =
    let
        relPoint =
            pointRelativeToCenter viewport point

        _ =
            Debug.log "center" { x = viewport.viewport.x / 2, y = viewport.viewport.y / 2 }

        _ =
            Debug.log "click" relPoint

        _ =
            Debug.log "player" user.pos

        _ =
            Debug.log "diff" diff

        ( poxX, posY ) =
            ( getX user.pos, getY user.pos )

        diff =
            Vec2.minus user.pos (vec2 relPoint.x relPoint.y)

        ( velX, velY ) =
            ( getX user.velocity, getY user.velocity )

        velocityMagnitude =
            sqrt (velX ^ 2 + velY ^ 2)

        newVel =
            -- we create a new vector at the point clicked
            diff
                -- then convert it to a unit vector
                |> Vec2.normalize
                -- then scale it to have the same magnitude as before
                |> Vec2.scale velocityMagnitude
                -- then add it to the original velocity
                |> Vec2.plus user.velocity
                -- but keep it within some limits, don't want to go too fast
                |> Vec2.clamp ( -5, 5 ) ( -5, 5 )
    in
    { user | velocity = newVel }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click pos ->
            model
                |> mapUser (moveUser model.viewport pos)
                |> (\newModel -> ( newModel, Cmd.none ))

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

        ViewportChange viewport ->
            { model | viewport = Debug.log "viewport" viewport }
                |> (\newModel -> { newModel | bodies = initBodies newModel })
                |> (\finalModel -> ( finalModel, Cmd.none ))


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



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ Collage.Render.svgBox ( model.viewport.viewport.width - 10, model.viewport.viewport.height - 10 ) <| scene model
        ]


scene : Model -> Collage msg
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



-- SUBSCRIPTIONS


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , onResize (\h w -> ViewportChange (initViewport h w))
        , onAnimationFrameDelta Tick
        , onMouseDown decodeClickEvent
        ]


type alias Point =
    { x : Float
    , y : Float
    }


decodeClickEvent : Decoder Msg
decodeClickEvent =
    Decode.map2 Point
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        |> Decode.map Click



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
