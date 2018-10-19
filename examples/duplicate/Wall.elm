module Wall exposing (Meta, Model, Msg(..), collideWith, collideWithWall, drawBody, init, update, view, wallMeta)

import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles.Math2D exposing (mul2, plus)
import Collage exposing (..)
import Color exposing (..)
import PhysicsConsts exposing (e0)



-- MODEL


type alias Model =
    List (Body Meta)


type alias Meta =
    {}


wallMeta : Meta
wallMeta =
    {}


init : Float -> Float -> Model
init width height =
    bounds ( width, height ) 10 e0 ( 0, 0 ) wallMeta



-- bounds ( width-100, width-100) 10 e0 ( 0, 0 ) wallMeta
-- UPDATE


type Msg
    = Resize ( Int, Int )


update : Msg -> Model -> Model
update (Resize ( w, h )) model =
    init (toFloat w) (toFloat h)


collideWith : Model -> Body meta2 -> Body meta2
collideWith walls user =
    List.foldl collideWithWall user walls


{-| collideWithWall collides a body with a wall, and returns that body
-}
collideWithWall : Body Meta -> Body meta2 -> Body meta2
collideWithWall wall user =
    let
        collisionResult =
            Engine.collision wall user

        ( wall2, user2 ) =
            Engine.resolveCollision collisionResult wall user
    in
    user2



-- VIEW


view : Model -> List (Collage msg)
view model =
    List.map drawBody model


drawBody : Body Meta -> Collage msg
drawBody model =
    let
        ready =
            case model.shape of
                Bubble radius ->
                    group
                        [ circle radius
                            |> filled (uniform model.color)
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                    group
                        [ rectangle (w * 2) (h * 2) |> outlined (solid 1 (uniform black))
                        ]
    in
    Collage.shift model.pos ready
