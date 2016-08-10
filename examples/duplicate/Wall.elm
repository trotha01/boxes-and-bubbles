module Wall exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2,plus)
import Color exposing (..)
import Collage exposing (..)
import Time exposing (Time)

-- MODEL
type alias Model meta =
    List (Body meta)

-- todo: simplify this meta type
type alias Meta =
    {}

-- TODO: simplify meta
wallMeta: Meta
wallMeta = {}

init : Float -> Model Meta
init width = 
    bounds ( width-10, width-10) 10 e0 ( 0, 0 ) wallMeta
    -- bounds ( width-100, width-100) 10 e0 ( 0, 0 ) wallMeta



-- UPDATE
type Msg
    = Tick Time

collideWith : Model Meta -> (Body meta2) -> (Body meta2)
collideWith walls user =
    List.foldl collideWithWall user walls

{-| collideWithWall collides a body with a wall, and returns that body
-}
collideWithWall : Body Meta -> (Body meta2) -> Body meta2
collideWithWall wall user =
    let collisionResult =
            Engine.collision wall user
        (wall2, user2) = (Engine.resolveCollision collisionResult wall user)
     in user2

-- VIEW
view : Model Meta -> List Form
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

noGravity t = ( ( 0, 0.0 ), ( 0, 0 ) )


e0 : Float
e0 =
    0.8
