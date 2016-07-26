module User exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2,plus)
import Color exposing (..)
import Collage exposing (..)
import Time exposing (Time)
import Keyboard.Extra as Keyboard

-- MODEL
type alias Model meta =
    Body meta

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
    bubble purple 100 1 e0 ( -80, 0 ) ( 1, 0 ) meta

-- UPDATE
type Msg
    = Tick Time
    | KeyPress Keyboard.Msg

update : Msg -> ( Model Meta, Keyboard.Model ) -> ( ( Model Meta, Keyboard.Model ), Cmd Keyboard.Msg )
update msg ( model, keyboard ) =
    case msg of
        Tick dt ->
            let newUser =
                    (uncurry Engine.update (noGravity dt)) model
            in ((newUser, keyboard), Cmd.none)

        KeyPress keyMsg ->
            let
                ( kybrd, keyboardCmd ) =
                    Keyboard.update keyMsg keyboard

                direction =
                    Keyboard.arrows kybrd

                updatedUser =
                    Body.move model direction
            in
                ( ( updatedUser, keyboard ), keyboardCmd )

{-| swallow: swallow another body
-}
swallow : Body Meta -> Body Meta -> ( Body Meta, Body Meta )
swallow user food =
    (user, {food|meta={meta|isFood=True}})

{-| collideWithBody: collide user with another body
-}
collideWithBody : Model Meta -> Body Meta -> (Model Meta, Body Meta)
collideWithBody user body =
    let
        collisionResult =
            Engine.collision user body

        (user1, body1) = if collisionResult.penetration > 0
            then case body.shape of
                -- if the penetration is greater than 2r, then the food is moving around inside
                -- if the penetration is less than r, then the food is still being swallowed
                (Bubble r) ->
                    if collisionResult.penetration > (r*2) || collisionResult.penetration < r
                    then swallow user body
                    else (Engine.resolveCollision collisionResult user body)
                (Box _) ->
                    Engine.resolveCollision collisionResult user body
            else
                Engine.resolveCollision collisionResult user body
    in
        ( user1, body1 )

{-| collideWithBodies: collide user with list of body
-}
collideWithBodies : Model Meta -> List (Body Meta) -> (Model Meta, List (Body Meta))
collideWithBodies user0 bodies0 =
    let (user1, bodies1) = List.foldl
            (\b ( u, bs ) ->
                let
                    ( u2, b2 ) =
                        collideWithBody u b
                in
                    ( u2, b2 :: bs )
            )
            ( user0, [] )
            bodies0
    in (user1, bodies1)


-- VIEW
view : Model meta -> Form
view model =
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