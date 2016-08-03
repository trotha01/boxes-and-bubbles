module Transform exposing (main)

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
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra as Keyboard

{-
TODO: make user larger when they eat food
auto generate random shapes
point user in specific direction (to help wall bounce flow)
-}

inf : Float
inf =
    1 / 0



-- infinity, hell yeah

e0 : Float
e0 =
    0.8



-- default restitution coefficient
-- box: (w,h) pos velocity density restitution
-- bubble: radius pos velocity density restitution


type alias Model meta =
    List (Body meta)

type alias Meta = Bool

{- meta is used to tell if the body has been eaten -}
meta : Bool
meta = False

(height, width) =
    (600, 600)

bColor : Color
bColor = yellow

boxColor : Color
boxColor = orange

someBodies : List (Body Meta)
someBodies =
    [ bubble bColor 20 1 e0 ( -80, 0 ) ( -1.5, 0 ) meta
    , bubble bColor 15 1 e0 ( 0, 200 ) ( -0.4, 1.0 ) meta
    , bubble bColor 5 1 e0 ( 200, -200 ) ( -1, -1 ) meta
    , bubble bColor 15 5 0.4 ( 100, 100 ) ( 1, 1 ) meta
    , bubble bColor 10 1 e0 ( 200, 200 ) ( 1, -1 ) meta
    , box boxColor ( 10, 10 ) 1 e0 ( 200, 0 ) ( 0, 0 ) meta
    , box boxColor ( 20, 20 ) 1 e0 ( -200, 0 ) ( 3, 0 ) meta
    , box boxColor ( 15, 15 ) 1 e0 ( 200, -200 ) ( -1, -1 ) meta
    ]
        ++ bounds ( width-50, height-50) 1 e0 ( 0, 0 ) meta

user : Body Meta
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
                            [ rect (w * 2) (h * 2) |> filled color
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

swallow : Body Meta -> Body Meta -> ( Body Meta, Body Meta )
swallow user food =
    (user, {food|meta=True})

{-| collide assumes a0 is the user, b0 is possible food
-}
collide : Body Meta -> Body Meta -> ( Body Meta, Body Meta )
collide a0 b0 =
    let
        collisionResult =
            Engine.collision a0 b0

        (a1, b1) = if collisionResult.penetration > 0
            then case b0.shape of
                -- if the penetration is greater than 2r, then the food is moving around inside
                -- if the penetration is less than r, then the food is still being swallowed
                (Bubble r) -> if collisionResult.penetration > r*2 || collisionResult.penetration < r
                    then swallow a0 b0
                    else (Engine.resolveCollision collisionResult a0 b0)
                (Box _) -> Engine.resolveCollision collisionResult a0 b0
            else
                Engine.resolveCollision collisionResult a0 b0
    in
        ( a1, b1 )


collideUser : Body Meta -> Model Meta -> ( Body Meta, Model Meta )
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

circArea : Float -> Float
circArea r =
    pi * r * r

combineShapes : Body Meta -> Body Meta -> Body Meta
combineShapes a0 b0 =
    let combined = 
        case (a0.shape, b0.shape) of
            (Bubble r1, Bubble r2) ->
                let a1 = circArea r1
                    a2 = circArea r2
                    boxSide = sqrt ((a1 + a2)/2)
                    in { a0 | shape = Box (boxSide, boxSide), color = boxColor }
            _ -> { a0 | shape = Box (15, 15) } -- we should never hit this case, only circles are food
     in {combined|meta = False}

collideBodyWith : Body Meta -> List (Body Meta) -> List (Body Meta) -> List (Body Meta)
collideBodyWith a0 bodies acc =
    case bodies of
        [] ->
            a0 :: acc

        b0 :: bs ->
            let
                collisionResult =
                    Engine.collision a0 b0

                in if collisionResult.penetration > 0
                        && a0.meta == True
                        && b0.meta == True
                then 
                    let combined = combineShapes a0 b0
                     in collideBodyWith combined bs (acc)
                else
                    let ( a1, b1 ) =
                        Engine.resolveCollision collisionResult a0 b0
                    in
                        collideBodyWith a1 bs (b1 :: acc)

collideBodiesAcc  : List (Body Meta) -> List (Body Meta) -> List (Body Meta)
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

collideBodies : Float -> Model Meta -> Model Meta
collideBodies dt bodies =
    -- uncurry step (noGravity dt) bodies
    List.map (uncurry Engine.update (noGravity dt)) (collideBodiesAcc [] bodies)

update : Msg -> ( Body Meta, Model Meta, Keyboard.Model ) -> ( ( Body Meta, Model Meta, Keyboard.Model ), Cmd Msg )
update msg ( user, bodies, keyboard ) =
    case msg of
        Tick dt ->
            let
                ( collidedUser, userCollidedBodies ) =
                    collideUser user bodies

                newUser =
                    (uncurry Engine.update (noGravity dt)) collidedUser

                collidedBodies =
                    collideBodies dt userCollidedBodies
            in
                ( ( newUser, (collidedBodies), keyboard ), Cmd.none )

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
            { init = ( ( user, someBodies, keyboard ), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }
