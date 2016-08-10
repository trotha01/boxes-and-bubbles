module Bound exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, plus)
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


boundMeta : Meta
boundMeta =
    {}


init : Float -> Float -> Model Meta
init width height =
    bounds ( width + 400, height + 400 ) 10 e0 ( 0, 0 ) boundMeta



-- bounds ( width-20, height-20) 10 e0 ( 0, 0 ) boundMeta
-- UPDATE


type Msg meta
    = Tick Time
    | Regenerate (Body meta)


{-| collideWithBodies: collide bounds with list of body
-}
collideWithBodies : Model Meta -> List (Body meta) -> ( List (Body meta), List (Msg meta) )
collideWithBodies bounds bodies =
    List.foldl
        (\body ( bodyAcc, msgsAcc ) ->
            collideBoundsWithBody bounds (Just body) ( bodyAcc, msgsAcc )
        )
        ( [], [] )
        bodies


{-| We return a List Msg and List Body, to make accumulating them easier
-}
collideBoundsWithBodies : Model Meta -> List (Body meta) -> ( List (Body meta), List (Msg meta) ) -> ( List (Body meta), List (Msg meta) )
collideBoundsWithBodies bounds bodies ( bodyAcc, msgAcc ) =
    case ( bounds, bodies ) of
        ( [], _ ) ->
            ( bodyAcc, msgAcc )

        ( _, [] ) ->
            ( bodyAcc, msgAcc )

        ( bound :: bnds, body :: bds ) ->
            let
                collisionResult =
                    Engine.collision bound body
            in
                if collisionResult.penetration > 0 then
                    ( bodyAcc, (Regenerate body) :: msgAcc )
                else
                    collideBoundsWithBodies bnds bds ( bodyAcc, msgAcc )


collideBoundWithBodies bound bodies ( bodyAcc, msgAcc ) =
    case bodies of
        [] ->
            []

        body :: bs ->
            let
                collisionResult =
                    Engine.collision bound body
            in
                if collisionResult.penetration > 0 then
                    collideBoundWithBodies bound bs ( bodyAcc, (Regenerate body) :: msgAcc )
                else
                    collideBoundWithBodies bound (body :: bs) ( bodyAcc, msgAcc )



-- this will have no base case


collideBoundsWithBody : List (Body Meta) -> Maybe (Body meta) -> ( List (Body meta), List (Msg meta) ) -> ( List (Body meta), List (Msg meta) )
collideBoundsWithBody bounds maybeBody ( bodyAcc, msgAcc ) =
    case ( bounds, maybeBody ) of
        ( [], Just body ) ->
            ( body :: bodyAcc, msgAcc )

        ( [], Nothing ) ->
            ( bodyAcc, msgAcc )

        ( _, Nothing ) ->
            ( bodyAcc, msgAcc )

        ( bound :: bs, Just body ) ->
            let
                collisionResult =
                    Engine.collision bound body
            in
                if collisionResult.penetration > 0 then
                    collideBoundsWithBody bs Nothing ( bodyAcc, (Regenerate body) :: msgAcc )
                else
                    collideBoundsWithBody bs (Just body) ( bodyAcc, msgAcc )



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


noGravity t =
    ( ( 0, 0.0 ), ( 0, 0 ) )


e0 : Float
e0 =
    0.8
