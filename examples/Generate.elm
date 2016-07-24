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
import BoxesAndBubbles.Math2D exposing (mul2,plus)
import List exposing (map)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Random

{-
TODO: make user larger when they eat food
auto generate random shapes
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
    { bodies: List (Body meta)
    , seed: Random.Seed
    }

type alias Meta =
    { isFood: Bool
    , isWall: Bool
    , isBound: Bool
    , dir: BoxesAndBubbles.Math2D.Vec2 
    }

initialModel : Model Meta
initialModel =
    { bodies = someBodies,
      seed = Random.initialSeed 3
    }

{- meta is used to tell if the body has been eaten -}
meta : Meta
meta = Meta False False False (0,0)

wallMeta: Meta
wallMeta = Meta False True False (0,0)

boundMeta: Meta
boundMeta = Meta False False True (0,0)

(height, width) =
    (700, 700)

bColor : Color
bColor = rgb 238 130 238 

boxColor : Color
boxColor = lightBlue

(bubbleCount, boxCount) = (20,10)

randBubbles : Random.Generator (List (Body Meta))
randBubbles =
    Random.list bubbleCount (randBubble bColor e0 (-200,200) (-3,3) meta)

randBoxes : Random.Generator (List (Body Meta))
randBoxes =
    Random.list boxCount (randBox boxColor e0 (-200,200) (5, 30) meta)

randBody : Random.Generator (Body Meta)
randBody =
    Random.bool `Random.andThen` (\coin ->
                if coin
                then randBubble bColor e0 (-200,200) (-3,3) meta
                else randBox boxColor e0 (-200,200) (5, 30) meta)

someBodies : List (Body Meta)
someBodies =
    let (bubbles, seed2) =
            Random.step randBubbles (Random.initialSeed 2)
        (boxes, seed3) =
            Random.step randBoxes seed2
        in bubbles
        ++ boxes
        ++ bounds ( width-10, width-10) 10 e0 ( 0, 0 ) wallMeta
        ++ bounds ( width+300, height+300) 10 e0 ( 0, 0 ) boundMeta

user : Body Meta
user =
    bubble purple 100 1 e0 ( -80, 0 ) ( 1, 0 ) meta


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
                        -- , veloLine
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
scene ( user, model, keyboard ) =
    collage width height <| map drawBody (user :: model.bodies)



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
    (user, {food|meta={meta|isFood=True}})

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
                (Bubble r) ->
                    if collisionResult.penetration > r*2 || collisionResult.penetration < r
                    then swallow a0 b0
                    else (Engine.resolveCollision collisionResult a0 b0)
                (Box _) ->
                    Engine.resolveCollision collisionResult a0 b0
            else
                Engine.resolveCollision collisionResult a0 b0
    in
        ( a1, b1 )


collideUser : Body Meta -> Model Meta -> ( Body Meta, Model Meta )
collideUser user model =
    let (user, newBodies) = List.foldl
            (\b ( u, bs ) ->
                let
                    ( u2, b2 ) =
                        collide u b
                in
                    ( u2, b2 :: bs )
            )
            ( user, [] )
            model.bodies
    in (user, {model|bodies = newBodies})

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
     in {combined|meta={meta|isFood=False}}

{-| regenerate is used when a body has reached the bounds
it regenerates a new body at the opposite end
-}
regenerate : Float -> Body Meta -> Body Meta
regenerate dt body =
    let (newBody, _) = (Random.step randBody (Random.initialSeed (round dt)))
        _ = Debug.log "time" dt
    in { newBody |  pos = mul2 body.pos (-15/16), velocity = plus (0, 0.2) (mul2 body.velocity (1/2))}

collideBodyWith : Float -> Body Meta -> List (Body Meta) -> List (Body Meta) -> (List (Body Meta))
collideBodyWith dt a0 bodies acc =
    case bodies of
        [] ->
            (a0 :: acc)

        b0 :: bs ->
            let
                collisionResult =
                    Engine.collision a0 b0

                in if collisionResult.penetration > 0
                        && a0.meta.isFood == True
                        && b0.meta.isFood == True
                then -- combine the food. TODO: create a new object when this happens
                    let combined = combineShapes a0 b0
                     in collideBodyWith dt combined bs (acc)
                else if collisionResult.penetration > 0 -- let bodies through the wall
                    && (a0.meta.isWall == True || b0.meta.isWall == True)
                    then collideBodyWith dt a0 bs (b0 :: acc)
                else if collisionResult.penetration > 0 -- recreate objects when they dissapear
                    && (a0.meta.isBound == True || b0.meta.isBound == True)
                    then -- move object to opposite side
                        if a0.meta.isBound
                        then collideBodyWith dt a0 bs ((regenerate dt b0) :: acc)
                        else collideBodyWith dt (regenerate dt a0) bs (b0 :: acc)

                else
                    let ( a1, b1 ) =
                        Engine.resolveCollision collisionResult a0 b0
                    in
                        collideBodyWith dt a1 bs (b1 :: acc)

collideBodiesAcc  : Float -> List (Body Meta) -> List (Body Meta) -> (List (Body Meta))
collideBodiesAcc dt acc bodies =
    case bodies of
        [] ->
            (acc)

        h :: t ->
            case collideBodyWith dt h t [] of
                [] ->
                    ([])

                (h1 :: t1) ->
                    collideBodiesAcc dt (h1 :: acc) t1

collideBodies : Float -> Model Meta -> (Model Meta)
collideBodies dt model =
    let (collidedBodies) = collideBodiesAcc dt [] model.bodies
        newBodies = List.map (uncurry Engine.update (noGravity dt)) collidedBodies
    in  ({model|bodies = newBodies})

update : Msg -> ( Body Meta, Model Meta, Keyboard.Model ) -> ( ( Body Meta, Model Meta, Keyboard.Model ), Cmd Msg )
update msg ( user, bodies, keyboard ) =
    case msg of
        Tick dt ->
            let
                ( collidedUser, userCollidedBodies ) =
                    collideUser user bodies

                newUser =
                    (uncurry Engine.update (noGravity dt)) collidedUser

                (collidedBodies) =
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
            { init = ( ( user, initialModel, keyboard ), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }
