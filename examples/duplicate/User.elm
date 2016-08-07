module User exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, plus)
import Color exposing (..)
import Collage exposing (..)
import Time exposing (Time)
import Keyboard.Extra as Keyboard


-- MODEL


type alias Model meta =
    Body meta



-- todo: simplify this meta type


type alias Meta =
    { isFood : Bool
    , isWall : Bool
    , isBound : Bool
    , dir : BoxesAndBubbles.Math2D.Vec2
    , ticks : Int
    }


meta : Meta
meta =
    Meta False False False ( 0, 0 ) 0


init : Model Meta
init =
    bubble purple 100 1 e0 ( -80, 0 ) ( 1, 0 ) meta



-- UPDATE


type Msg
    = Tick Time
    | KeyPress Keyboard.Msg


update : Msg -> ( Model Meta, Keyboard.Model ) -> ( ( Model Meta, List (Body meta), Keyboard.Model ), Cmd Keyboard.Msg )
update msg ( model, keyboard ) =
    case msg of
        Tick dt ->
            let
                model2 =
                    (uncurry Engine.update (noGravity dt)) model
            in
                ( ( model2, [], keyboard ), Cmd.none )

        KeyPress keyMsg ->
            let
                ( kybrd, keyboardCmd ) =
                    Keyboard.update keyMsg keyboard

                direction =
                    Keyboard.arrows kybrd

                updatedUser =
                    Body.move model direction
            in
                ( ( updatedUser, [], keyboard ), keyboardCmd )


type alias Food a =
    { a | isFood : Bool }


{-| swallow: turn a body into food
-}
swallow : Body (Food a) -> Body (Food a)
swallow food =
    let
        meta =
            food.meta

        meta' =
            { meta | isFood = True }
    in
        { food | meta = meta' }


{-| collideWithBody: collide user with another body
-}
collideWithBody : Model meta -> Body (Food a) -> ( Model meta, Body (Food a) )
collideWithBody user body =
    let
        collisionResult =
            Engine.collision user body

        ( user1, body1 ) =
            if collisionResult.penetration > 0 then
                case body.shape of
                    -- if the penetration is greater than 2r, then the food is moving around inside
                    -- if the penetration is less than r, then the food is still being swallowed
                    Bubble r ->
                        if collisionResult.penetration < r then
                            let
                                cr =
                                    { collisionResult | normal = mul2 collisionResult.normal -1 }

                                ( user1, body1 ) =
                                    (Engine.resolveCollision cr user body)
                            in
                                ( user1, swallow body1 )
                        else if
                            collisionResult.penetration > (r * 2)
                            -- || collisionResult.penetration < r
                        then
                            ( user, swallow body )
                        else
                            (Engine.resolveCollision collisionResult user body)

                    Box _ ->
                        Engine.resolveCollision collisionResult user body
            else
                Engine.resolveCollision collisionResult user body
    in
        ( user1, body1 )


{-| collideWithBodies: collide user with list of body
-}
collideWithBodies : Model meta -> List (Body (Food a)) -> ( Model meta, List (Body (Food a)) )
collideWithBodies user0 bodies0 =
    let
        ( user1, bodies1 ) =
            List.foldl
                (\b ( u, bs ) ->
                    let
                        ( u2, b2 ) =
                            collideWithBody u b
                    in
                        ( u2, b2 :: bs )
                )
                ( user0, [] )
                bodies0
    in
        ( user1, bodies1 )



-- VIEW


view : Model meta -> Form
view model =
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
