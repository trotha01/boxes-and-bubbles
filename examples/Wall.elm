module Wall exposing (..)

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

-- TODO: simplify meta
wallMeta: Meta
wallMeta = Meta False True False (0,0)

init : Float -> Model Meta
init width = 
    bounds ( width-10, width-10) 10 e0 ( 0, 0 ) wallMeta



-- UPDATE
type Msg
    = Tick Time

collideWith : Model Meta -> List (Body Meta) -> (Model Meta, List (Body Meta))
collideWith model bodies
    = (model, bodies)


collideWithWall : Body Meta -> List (Body Meta) -> (Body Meta, List (Body Meta))
collideWithWall wall bodies
    = (wall, bodies)

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