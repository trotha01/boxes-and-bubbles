module Bound exposing (Meta, Model, Msg(..), boundMeta, collideBoundWithBodies, collideBoundsWithBodies, collideBoundsWithBody, collideWithBodies, drawBody, init, update, view)

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



-- todo: simplify this meta type


type alias Meta =
    {}


boundMeta : Meta
boundMeta =
    {}


init : Float -> Float -> Model
init width height =
    bounds ( width + 400, height + 400 ) 10 e0 ( 0, 0 ) boundMeta



-- bounds ( width-20, height-20) 10 e0 ( 0, 0 ) boundMeta
-- UPDATE


type Msg meta
    = Tick Float
    | Regenerate (Body meta)
    | Resize ( Int, Int )


update : Msg a -> Model -> Model
update msg model =
    case msg of
        Resize ( w, h ) ->
            init (toFloat w) (toFloat h)

        _ ->
            model


{-| collideWithBodies: collide bounds with list of body
-}
collideWithBodies : Model -> List (Body meta) -> ( List (Body meta), List (Msg meta) )
collideWithBodies bounds bodies =
    List.foldl
        (\body ( bodyAcc, msgsAcc ) ->
            collideBoundsWithBody bounds (Just body) ( bodyAcc, msgsAcc )
        )
        ( [], [] )
        bodies


{-| We return a List Msg and List Body, to make accumulating them easier
-}
collideBoundsWithBodies : Model -> List (Body meta) -> ( List (Body meta), List (Msg meta) ) -> ( List (Body meta), List (Msg meta) )
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
                ( bodyAcc, Regenerate body :: msgAcc )

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
                collideBoundWithBodies bound bs ( bodyAcc, Regenerate body :: msgAcc )

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
                collideBoundsWithBody bs Nothing ( bodyAcc, Regenerate body :: msgAcc )

            else
                collideBoundsWithBody bs (Just body) ( bodyAcc, msgAcc )



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
