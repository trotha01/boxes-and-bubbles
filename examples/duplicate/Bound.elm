module Bound exposing (..)

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
    { isFood: Bool
    , isWall: Bool
    , isBound: Bool
    , dir: BoxesAndBubbles.Math2D.Vec2 
    }

-- TODO: simplify meta
boundMeta: Meta
boundMeta = Meta False False True (0,0)

init : Float -> Float -> Model Meta
init width height = 
    bounds ( width+300, height+300) 10 e0 ( 0, 0 ) boundMeta



-- UPDATE
type Msg
    = Tick Time
    | Regenerate (Body Meta)


{-| collideWithBodies: collide bounds with list of body
-}
collideWithBodies : Model Meta -> List (Body Meta) -> (List (Body Meta), List Msg)
collideWithBodies bounds bodies =
    List.foldl
        (\body (bodyAcc, msgsAcc) ->
            collideBoundsWithBody bounds (Just body) (bodyAcc, msgsAcc)
        )
        ([], [])
        bodies

{-| We return a List Msg and List Body, to make accumulating them easier
-}
collideBoundsWithBodies : Model Meta -> List (Body Meta) -> (List (Body Meta), List Msg) -> (List (Body Meta), List Msg)
collideBoundsWithBodies bounds bodies (bodyAcc, msgAcc) =
    case (bounds, bodies) of
        ([], _) -> (bodyAcc, msgAcc)
        (_, []) -> (bodyAcc, msgAcc)
        (bound :: bnds, body :: bds) ->
            let collisionResult = Engine.collision bound body
            in if collisionResult.penetration > 0
                then (bodyAcc, (Regenerate body) :: msgAcc)
                else collideBoundsWithBodies bnds bds (bodyAcc, msgAcc)     


-- should we collide
--    bound  & bodies
-- or bounds & body  ?
collideBoundWithBodies bound bodies (bodyAcc, msgAcc) =
    case bodies of
        [] -> []
        body :: bs ->
            let collisionResult = Engine.collision bound body
            in if collisionResult.penetration > 0
                then collideBoundWithBodies bound bs (bodyAcc, (Regenerate body) :: msgAcc)
                else collideBoundWithBodies bound (body :: bs) (bodyAcc, msgAcc)  -- this will have no base case

collideBoundsWithBody : List (Body Meta) -> Maybe (Body Meta) -> (List (Body Meta), List Msg) -> (List (Body Meta), List Msg)
collideBoundsWithBody bounds maybeBody (bodyAcc, msgAcc) =
    case (bounds, maybeBody) of
        ([], Just body) -> (body :: bodyAcc, msgAcc)
        ([], Nothing) -> (bodyAcc, msgAcc)
        (_, Nothing) -> (bodyAcc, msgAcc)
        (bound :: bs, Just body) ->
            let collisionResult = Engine.collision bound body
            in if collisionResult.penetration > 0
                then collideBoundsWithBody bs Nothing (bodyAcc, (Regenerate body) :: msgAcc)
                else collideBoundsWithBody bs (Just body) (bodyAcc, msgAcc)


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
