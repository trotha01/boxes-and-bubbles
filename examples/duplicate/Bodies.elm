module Bodies exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2,plus)
import Color exposing (..)
import Collage exposing (..)
import Time exposing (Time)
import Random

-- MODEL
type alias Model meta =
    List (Body meta)

-- todo: simplify this meta type
type alias Meta =
    { isFood: Bool
    , eaten : Bool
    , isWall: Bool
    , isBound: Bool
    , dir: BoxesAndBubbles.Math2D.Vec2 
    }

meta : Meta
meta = Meta False False False False (0,0)

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

update : Msg -> Model Meta -> Model Meta
update (Tick dt) model =
    collideBodies dt model

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

collideBodyWith : Float -> Body Meta -> List (Body Meta) -> List (Body Meta) -> List (Body Meta)
collideBodyWith dt a0 bodies acc =
    case bodies of
        [] ->
            (a0 :: acc)

        b0 :: bs ->
            let
                collisionResult =
                    Engine.collision a0 b0
                in if collisionResult.penetration > 0
                        && a0.meta.eaten == True
                        && b0.meta.eaten == True
                then -- combine the food. TODO: create a new object from the side when this happens
                    let combined = combineShapes a0 b0
                     in collideBodyWith dt combined bs acc
                else
                    let ( a1, b1 ) =
                        Engine.resolveCollision collisionResult a0 b0
                    in
                        collideBodyWith dt a1 bs (b1 :: acc)

collideBodiesAcc  : Float -> List (Body Meta) -> List (Body Meta) -> List (Body Meta)
collideBodiesAcc dt acc bodies =
    case bodies of
        [] ->
            acc

        h :: t ->
            case collideBodyWith dt h t [] of
                [] ->
                    []

                (h1 :: t1) -> 
                    collideBodiesAcc dt (h1 :: acc) t1

collideBodies : Float -> Model Meta -> Model Meta
collideBodies dt model =
    let collidedBodies = collideBodiesAcc dt [] model
        newBodies = List.map (uncurry Engine.update (noGravity dt)) collidedBodies
    in  newBodies

-- VIEW
view : Model meta -> List Form
view model =
    List.map drawBody model

drawBody : Body meta -> Form
drawBody model =
        let
        veloLine =
            segment ( 0, 0 ) (mul2 model.velocity 5) |> traced (solid red)
        ready =
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
    in
        Collage.move model.pos ready


-- helpers

noGravity t = ( ( 0, 0.0 ), ( 0, 0 ) )


e0 : Float
e0 =
    0.8
