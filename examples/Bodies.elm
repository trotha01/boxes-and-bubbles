module Bodies exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2,plus)
import Color exposing (..)
import Collage exposing (..)
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Random

-- MODEL
type alias Model meta =
    List (Body meta)

-- todo: simplify this meta type
type alias Meta =
    { isFood: Bool
    , isWall: Bool
    , isBound: Bool
    , dir: BoxesAndBubbles.Math2D.Vec2 
    }

meta : Meta
meta = Meta False False False (0,0)

init : Model Meta
init = 
    someBodies


(bubbleCount, boxCount) = (20,10)
bColor : Color
bColor = rgb 238 130 238 

boxColor : Color
boxColor = lightBlue

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


-- UPDATE
type Msg
    = Tick Time
    | KeyPress Keyboard.Msg

{-| regenerate is used when a body has reached the bounds
it regenerates a new body at the opposite end
-}
regenerate : Random.Seed -> Body Meta -> (Body Meta, Random.Seed)
regenerate seed body =
    let (newBody, seed') = (Random.step randBody seed)
    in ({ newBody |  pos = mul2 body.pos (-15/16), velocity = plus (0, 0.2) (mul2 body.velocity (1/2))}
        , seed)

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

collideBodyWith : Float -> Body Meta -> List (Body Meta) -> (List (Body Meta), Cmd Msg) -> (List (Body Meta), Cmd Msg)
collideBodyWith dt a0 bodies (acc, accCmd) =
    case bodies of
        [] ->
            ((a0 :: acc), accCmd)

        b0 :: bs ->
            let
                collisionResult =
                    Engine.collision a0 b0

                in if collisionResult.penetration > 0
                        && a0.meta.isFood == True
                        && b0.meta.isFood == True
                then -- combine the food. TODO: create a new object from the side when this happens
                    let combined = combineShapes a0 b0
                     in collideBodyWith dt combined bs (acc, accCmd)
                else if collisionResult.penetration > 0 -- let bodies through the wall
                    && (a0.meta.isWall == True || b0.meta.isWall == True)
                    then collideBodyWith dt a0 bs ((b0 :: acc), accCmd)
                else if collisionResult.penetration > 0 -- recreate objects when they dissapear
                    && (a0.meta.isBound == True || b0.meta.isBound == True)
                    then -- move object to opposite side
                        if a0.meta.isBound
                        -- then collideBodyWith dt a0 bs ( acc, Cmd.batch [accCmd, Cmd.map (\_ -> RegenerateBody b0) Cmd.none]) -- TODO: find the right way to make a cmd
                        -- else collideBodyWith dt b0 bs (acc, Cmd.batch [accCmd, Cmd.map (\_ -> RegenerateBody a0) Cmd.none])
                        then 
                            let (regenerated, _) = (regenerate (Random.initialSeed (round dt)) b0)
                             in collideBodyWith dt a0 bs ((regenerated :: acc), accCmd)
                        else
                            let (regenerated, _) = (regenerate (Random.initialSeed (round dt)) a0)
                             in collideBodyWith dt regenerated bs ((b0 :: acc), accCmd)

                else
                    let ( a1, b1 ) =
                        Engine.resolveCollision collisionResult a0 b0
                    in
                        collideBodyWith dt a1 bs ((b1 :: acc), accCmd)

collideBodiesAcc  : Float -> (List (Body Meta), Cmd Msg) -> List (Body Meta) -> (List (Body Meta), Cmd Msg)
collideBodiesAcc dt (acc, accCmd) bodies =
    case bodies of
        [] ->
            (acc, accCmd)

        h :: t ->
            case collideBodyWith dt h t ([], Cmd.none) of
                ([], cmd) ->
                    ([], cmd)

                ((h1 :: t1), cmd) -> -- TODO: handle this command
                    collideBodiesAcc dt ((h1 :: acc), accCmd) t1

collideBodies : Float -> Model Meta -> (Model Meta, Cmd Msg)
collideBodies dt model =
    let (collidedBodies, cmd) = collideBodiesAcc dt ([], Cmd.none) model
        newBodies = List.map (uncurry Engine.update (noGravity dt)) collidedBodies
    in  (newBodies, cmd)

-- VIEW
view : Model meta -> List Form
view model =
    List.map drawBody model

drawBody : Body meta -> Form
drawBody model =
        let
        veloLine =
            segment ( 0, 0 ) (mul2 model.velocity 5) |> traced (solid red)
        in
            case model.shape of
                Bubble radius ->
                    group
                        [ circle radius
                            |> filled model.color
                        -- , veloLine
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                        group
                            [ rect (w * 2) (h * 2) |> filled model.color
                            ]



-- helpers

noGravity t = ( ( 0, 0.0 ), ( 0, 0 ) )


e0 : Float
e0 =
    0.8