module Example exposing (main)

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
import BoxesAndBubbles.Math2D exposing (mul2,plus)
import List exposing (map)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Random
import User exposing (init)
import Bodies

{-
TODO: make user larger when they eat food
use seed when randomly making shapes
find out how to more evenly distribute the shapes
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

-- MODEL
type alias Model meta =
    { bodies: List (Body meta)
    , seed: Random.Seed
    , user: User.Model meta
    }

type alias Meta =
    { isFood: Bool
    , isWall: Bool
    , isBound: Bool
    , dir: BoxesAndBubbles.Math2D.Vec2 
    }

initialModel : Model Meta
initialModel =
    { bodies = someBodies
    ,  seed = Random.initialSeed 3
    ,  user = User.init
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
    Random.list boxCount (randBox boxColor e0 (-200,200) (10, 30) meta)

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

-- VIEW

scene : ( Model meta, Keyboard.Model ) -> Element
scene ( model, keyboard ) =
    collage width height 
        <| ( (User.view model.user) :: Bodies.view model.bodies )



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

-- UPDATE

type Msg
    = Tick Time
    | KeyPress Keyboard.Msg
    | RegenerateBody (Body Meta)
    | BodiesMsg (Bodies.Msg)


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , AnimationFrame.diffs Tick
        ]


{-| regenerate is used when a body has reached the bounds
it regenerates a new body at the opposite end
-}
regenerate : Random.Seed -> Body Meta -> (Body Meta, Random.Seed)
regenerate seed body =
    let (newBody, seed') = (Random.step randBody seed)
    in ({ newBody |  pos = mul2 body.pos (-15/16), velocity = plus (0, 0.2) (mul2 body.velocity (1/2))}
        , seed)

update : Msg -> ( Model Meta, Keyboard.Model ) -> ( ( Model Meta, Keyboard.Model ), Cmd Msg )
update msg ( model, keyboard ) =
    case msg of
        Tick dt ->
            let
                ((user1, _), _) = -- update user per keyboard presses
                    User.update (User.Tick dt) (model.user, keyboard)

                -- collide user with the bodies
                (user2, bodies2) = User.collideWithBodies user1 model.bodies


                (bodies3, cmd) = -- update the body collisions
                    Bodies.update (Bodies.Tick dt) bodies2

            in
                ( ( {model| user = user2, bodies = bodies3 }, keyboard ), Cmd.map BodiesMsg cmd )

        KeyPress keyMsg ->
            let
                ((updatedUser, keyboard), keyboardCmd) = User.update (User.KeyPress keyMsg) (model.user, keyboard) 
            in
                ( ( {model | user = updatedUser}, keyboard ), Cmd.map KeyPress keyboardCmd )
        RegenerateBody body ->
            let (newBody, newSeed) = regenerate model.seed body
                newModel = {model|bodies = model.bodies ++ [newBody], seed = newSeed}
                _ = Debug.log "regenerate" ""
            in ((newModel, keyboard), Cmd.none)
        BodiesMsg _ ->
            ((model, keyboard), Cmd.none)


{-| Run the animation started from the initial scene defined as `labeledBodies`.
-}
main : Program Never
main =
    let
        ( keyboard, keyboardCmd ) =
            Keyboard.init
    in
        program
            { init = ( ( initialModel, keyboard ), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }
