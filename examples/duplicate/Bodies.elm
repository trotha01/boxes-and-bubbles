module Bodies exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, plus)
import Color exposing (..)
import Collage exposing (..)
import Time exposing (Time)
import Random


-- MODEL


type alias Model =
    List (Body Meta)


type alias Meta =
    { isFood : Bool
    , eaten : Bool
    }


init =
    someBodies



-- UPDATE


type Msg
    = Tick Time
    | Points Int


update : Msg -> Model -> ( Model, List Msg )
update msg model =
    case msg of
        Tick dt ->
            collideBodies dt model

        _ ->
            ( model, [] )


collideBodies : Float -> Model -> ( Model, List Msg )
collideBodies dt model =
    let
        ( collidedBodies, msgs ) =
            collideBodiesAcc dt ( [], [] ) model

        newBodies =
            List.map (uncurry Engine.update (noGravity dt)) collidedBodies
    in
        ( newBodies, msgs )


collideBodiesAcc : Float -> ( Model, List Msg ) -> Model -> ( Model, List Msg )
collideBodiesAcc dt ( acc, accMsgs ) bodies =
    case bodies of
        [] ->
            ( acc, accMsgs )

        h :: t ->
            case collideBodyWith dt h t ( [], [] ) of
                ( [], msgs ) ->
                    ( [], accMsgs ++ msgs )

                ( h1 :: t1, msgs ) ->
                    collideBodiesAcc dt ( (h1 :: acc), accMsgs ++ msgs ) t1


collideBodyWith : Float -> Body Meta -> Model -> ( Model, List Msg ) -> ( Model, List Msg )
collideBodyWith dt a0 bodies ( acc, accMsg ) =
    case bodies of
        [] ->
            ( (a0 :: acc), accMsg )

        b0 :: bs ->
            let
                collisionResult =
                    Engine.collision a0 b0
            in
                if
                    (collisionResult.penetration > 0)
                        && (a0.meta.eaten == True)
                        && (b0.meta.eaten == True)
                then
                    -- combine the food. TODO: create a new object from the side when this happens
                    let
                        combined =
                            combineShapes a0 b0
                    in
                        collideBodyWith dt combined bs ( acc, (Points 10) :: accMsg )
                else
                    let
                        ( a1, b1 ) =
                            Engine.resolveCollision collisionResult a0 b0
                    in
                        collideBodyWith dt a1 bs ( (b1 :: acc), accMsg )


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
        { combined | meta = { meta | isFood = False } }



-- VIEW


view : Model -> List Form
view model =
    List.map drawBody model


drawBody : Body Meta -> Form
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


noGravity t =
    ( ( 0, 0.0 ), ( 0, 0 ) )


e0 : Float
e0 =
    0.8


circArea : Float -> Float
circArea r =
    pi * r * r


meta =
    { isFood = False
    , eaten = False
    }


food =
    { isFood = True
    , eaten = False
    }


bColor : Color
bColor =
    rgb 238 130 238


boxColor : Color
boxColor =
    lightBlue


( bubbleCount, boxCount ) =
    ( 20, 10 )
randBubbles : a -> Random.Generator (List (Body a))
randBubbles meta =
    Random.list bubbleCount (randBubble bColor e0 ( -200, 200 ) ( -3, 3 ) meta)


randBoxes : a -> Random.Generator (List (Body a))
randBoxes meta =
    Random.list boxCount (randBox boxColor e0 ( -200, 200 ) ( 10, 30 ) meta)


randBody : Random.Generator (Body Meta)
randBody =
    Random.bool
        `Random.andThen` (\coin ->
                            if coin then
                                randBubble bColor e0 ( -200, 200 ) ( -3, 3 ) food
                            else
                                randBox boxColor e0 ( 10, 50 ) ( 10, 50 ) meta
                         )


someBodies : List (Body Meta)
someBodies =
    let
        ( bubbles, seed2 ) =
            Random.step (randBubbles food) (Random.initialSeed 2)

        ( boxes, seed3 ) =
            Random.step (randBoxes meta) seed2
    in
        (bubbles ++ boxes)


{-| regenerate is used when a body has reached the bounds
it regenerates a new body at the opposite end
-}
regenerate : Random.Seed -> Body Meta -> ( Body Meta, Random.Seed )
regenerate seed body =
    let
        ( newBody, seed' ) =
            (Random.step (randBody) seed)
    in
        ( { newBody | pos = mul2 body.pos (-15 / 16), velocity = plus ( 0, 0.2 ) (mul2 body.velocity (1 / 2)) }
        , seed'
        )
