module User exposing (Children, Food, Meta, Model, Msg(..), childFromModel, collideWithBodies, collideWithBody, init, swallow, update, userMeta, view)

import Bodies
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles.Math2D exposing (mul2, plus)
import Collage exposing (..)
import Color exposing (..)
import Keyboard
import Keyboard.Arrows as Keyboard
import PhysicsConsts exposing (e0, noGravity)



-- MODEL


type alias Model =
    Body Meta


type alias Children =
    List (Body Bodies.Meta)


type alias Meta =
    { dir : BoxesAndBubbles.Math2D.Vec2
    }


userMeta : Meta
userMeta =
    { dir = ( 0, 0 ) }


init : Model
init =
    bubble purple 100 1 e0 ( -80, 0 ) ( 1, 0 ) userMeta



-- UPDATE


type Msg
    = Tick Float
    | KeyPress Keyboard.Msg


update : Msg -> ( Model, List Keyboard.Key ) -> ( ( Model, List Keyboard.Key ), Cmd msg )
update msg ( model, keyboard ) =
    case msg of
        Tick dt ->
            let
                ( gravity, ambiant ) =
                    noGravity dt

                newUser =
                    Engine.update gravity ambiant model
            in
            ( ( newUser, keyboard ), Cmd.none )

        KeyPress keyMsg ->
            let
                pressedKeys =
                    Keyboard.update keyMsg keyboard

                direction =
                    Keyboard.arrows pressedKeys

                user2 =
                    Body.move model direction
            in
            ( ( user2, pressedKeys ), Cmd.none )


childFromModel : Model -> Body Bodies.Meta
childFromModel model =
    { pos = model.pos
    , velocity = model.velocity
    , inverseMass = model.inverseMass
    , restitution = model.restitution
    , shape = model.shape
    , color = model.color
    , meta =
        { eaten = False
        , isFood = False
        }
    }


type alias Food a =
    { a | isFood : Bool, eaten : Bool }


{-| swallow: turn a body into food
-}
swallow : Body (Food a) -> Body (Food a)
swallow food =
    let
        meta =
            food.meta

        meta2 =
            { meta | eaten = True }
    in
    { food | meta = meta2 }


{-| collideWithBody: collide user with another body
-}
collideWithBody : Model -> Body (Food a) -> ( Model, Body (Food a) )
collideWithBody user body =
    let
        collisionResult =
            -- TODO: only collide if we have to
            Engine.collision user body

        ( user2, body2 ) =
            if collisionResult.penetration > 0 && body.meta.isFood then
                case body.shape of
                    -- if the penetration is greater than 2r, then the food is moving around inside
                    -- if the penetration is less than r, then the food is still being swallowed
                    Bubble r ->
                        if collisionResult.penetration < r then
                            let
                                cr =
                                    { collisionResult | normal = mul2 collisionResult.normal -1 }

                                ( user1, body1 ) =
                                    Engine.resolveCollision cr user body
                            in
                            ( user1, swallow body1 )

                        else if
                            collisionResult.penetration > (r * 2)
                            -- || collisionResult.penetration < r
                        then
                            ( user, swallow body )

                        else
                            Engine.resolveCollision collisionResult user body

                    Box _ ->
                        Engine.resolveCollision collisionResult user body

            else
                Engine.resolveCollision collisionResult user body
    in
    ( user2, body2 )


{-| collideWithBodies: collide user with list of body
-}
collideWithBodies : Model -> List (Body (Food a)) -> ( Model, List (Body (Food a)) )
collideWithBodies model bodies0 =
    List.foldl
        (\b ( u, bs ) ->
            let
                ( u2, b2 ) =
                    collideWithBody u b
            in
            ( u2, b2 :: bs )
        )
        ( model, [] )
        bodies0



-- VIEW


view : Model -> Collage msg
view user =
    let
        veloLine =
            segment ( 0, 0 ) (mul2 user.velocity 5) |> traced (solid 1 (uniform red))

        ready =
            case user.shape of
                Bubble radius ->
                    group
                        [ circle radius
                            |> filled (uniform user.color)
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
    Collage.shift user.pos ready
