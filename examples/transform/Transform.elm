module Transform exposing (main)

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



-- MODEL


type alias Model meta =
    { user : Body meta
    , bodies : List (Body meta)
    , keys : List Keyboard.Key
    }


type alias Meta =
    Bool



{- meta is used to tell if the body has been eaten -}


bodyMeta : Bool
bodyMeta =
    False


height =
    600


width =
    600


bColor : Color
bColor =
    yellow


boxColor : Color
boxColor =
    orange


someBodies : List (Body Meta)
someBodies =
    [ bubble bColor 20 1 e0 ( -80, 0 ) ( -1.5, 0 ) bodyMeta
    , bubble bColor 15 1 e0 ( 0, 200 ) ( -0.4, 1.0 ) bodyMeta
    , bubble bColor 5 1 e0 ( 200, -200 ) ( -1, -1 ) bodyMeta
    , bubble bColor 15 5 0.4 ( 100, 100 ) ( 1, 1 ) bodyMeta
    , bubble bColor 10 1 e0 ( 200, 200 ) ( 1, -1 ) bodyMeta
    , box boxColor ( 10, 10 ) 1 e0 ( 200, 0 ) ( 0, 0 ) bodyMeta
    , box boxColor ( 20, 20 ) 1 e0 ( -200, 0 ) ( 3, 0 ) bodyMeta
    , box boxColor ( 15, 15 ) 1 e0 ( 200, -200 ) ( -1, -1 ) bodyMeta
    ]
        ++ bounds ( width - 50, height - 50 ) 1 e0 ( 0, 0 ) bodyMeta


initialUser : Body Meta
initialUser =
    bubble blue 100 1 e0 ( -80, 0 ) ( 0, 0 ) bodyMeta



-- VIEW


scene : Model meta -> Collage msg
scene { user, bodies, keys } =
    group <| map drawBody (bodies ++ [ user ])


drawBody : Body meta -> Collage msg
drawBody { color, pos, velocity, inverseMass, restitution, shape, meta } =
    let
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
                        [ rectangle (w * 2) (h * 2) |> filled (uniform color)
                        ]
    in
    Collage.shift pos ready



-- UPDATE


type Msg
    = Tick Float
    | KeyPress Keyboard.Msg


update : Msg -> Model Meta -> ( Model Meta, Cmd Msg )
update msg { user, bodies, keys } =
    case msg of
        Tick dt ->
            let
                ( collidedUser, userCollidedBodies ) =
                    collideUser user bodies

                ( gravity, ambiant ) =
                    noGravity dt

                newUser =
                    Engine.update gravity ambiant collidedUser

                collidedBodies =
                    collideBodies dt userCollidedBodies
            in
            ( { user = newUser, bodies = collidedBodies, keys = keys }, Cmd.none )

        KeyPress keyMsg ->
            let
                pressedKeys =
                    Keyboard.update keyMsg keys

                direction =
                    Keyboard.arrows pressedKeys

                updatedUser =
                    Body.move user direction
            in
            ( { user = updatedUser, bodies = bodies, keys = pressedKeys }, Cmd.none )


collideUser : Body Meta -> List (Body Meta) -> ( Body Meta, List (Body Meta) )
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
collide : Body Meta -> Body Meta -> ( Body Meta, Body Meta )
collide a0 b0 =
    let
        collisionResult =
            Engine.collision a0 b0

        ( a1, b1 ) =
            if collisionResult.penetration > 0 then
                case b0.shape of
                    -- if the penetration is greater than 2r, then the food is moving around inside
                    -- if the penetration is less than r, then the food is still being swallowed
                    Bubble r ->
                        if collisionResult.penetration > r * 2 || collisionResult.penetration < r then
                            swallow a0 b0

                        else
                            Engine.resolveCollision collisionResult a0 b0

                    Box _ ->
                        Engine.resolveCollision collisionResult a0 b0

            else
                Engine.resolveCollision collisionResult a0 b0
    in
    ( a1, b1 )


swallow : Body Meta -> Body Meta -> ( Body Meta, Body Meta )
swallow user food =
    ( user, { food | meta = True } )


circArea : Float -> Float
circArea r =
    pi * r * r


combineShapes : Body Meta -> Body Meta -> Body Meta
combineShapes a0 b0 =
    let
        combined =
            case ( a0.shape, b0.shape ) of
                ( Bubble r1, Bubble r2 ) ->
                    let
                        a1 =
                            circArea r1

                        a2 =
                            circArea r2

                        boxSide =
                            sqrt ((a1 + a2) / 2)
                    in
                    { a0 | shape = Box ( boxSide, boxSide ), color = boxColor }

                _ ->
                    { a0 | shape = Box ( 15, 15 ) }

        -- we should never hit this case, only circles are food
    in
    { combined | meta = False }


collideBodyWith : Body Meta -> List (Body Meta) -> List (Body Meta) -> List (Body Meta)
collideBodyWith a0 bodies acc =
    case bodies of
        [] ->
            a0 :: acc

        b0 :: bs ->
            let
                collisionResult =
                    Engine.collision a0 b0
            in
            if
                collisionResult.penetration
                    > 0
                    && a0.meta
                    == True
                    && b0.meta
                    == True
            then
                let
                    combined =
                        combineShapes a0 b0
                in
                collideBodyWith combined bs acc

            else
                let
                    ( a1, b1 ) =
                        Engine.resolveCollision collisionResult a0 b0
                in
                collideBodyWith a1 bs (b1 :: acc)


collideBodiesAcc : List (Body Meta) -> List (Body Meta) -> List (Body Meta)
collideBodiesAcc acc bodies =
    case bodies of
        [] ->
            acc

        h :: t ->
            case collideBodyWith h t [] of
                [] ->
                    []

                h1 :: t1 ->
                    collideBodiesAcc (h1 :: acc) t1


collideBodies : Float -> List (Body Meta) -> List (Body Meta)
collideBodies dt bodies =
    let
        ( gravity, ambiant ) =
            noGravity dt
    in
    List.map (Engine.update gravity ambiant) (collideBodiesAcc [] bodies)



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


initialModel : Model Meta
initialModel =
    { user = initialUser, bodies = someBodies, keys = [] }


init : Flags -> ( Model Meta, Cmd Msg )
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
