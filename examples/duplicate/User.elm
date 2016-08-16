module User exposing (..)

import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles.Engine as Engine
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, plus)
import Color exposing (..)
import Collage exposing (..)
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Bodies
import PhysicsConsts exposing (noGravity, e0)


-- MODEL


type alias Model =
    Body Meta


type alias Children =
    List (Body Bodies.Meta)


type alias Meta =
    { dir : BoxesAndBubbles.Math2D.Vec2
    }


meta : Meta
meta =
    { dir = ( 0, 0 ) }


init : Model
init =
    bubble purple 100 1 e0 ( -80, 0 ) ( 1, 0 ) meta



-- UPDATE


type Msg
    = Tick Time
    | KeyPress Keyboard.Msg


update : Msg -> ( Model, Keyboard.Model ) -> ( ( Model, Keyboard.Model ), Cmd Keyboard.Msg )
update msg ( model, keyboard ) =
    case msg of
        Tick dt ->
            let
                user2 =
                    (uncurry Engine.update (noGravity dt)) model
            in
                ( ( user2, keyboard ), Cmd.none )

        KeyPress keyMsg ->
            let
                ( kybrd, keyboardCmd ) =
                    Keyboard.update keyMsg keyboard

                direction =
                    Keyboard.arrows kybrd

                user2 =
                    Body.move model direction
            in
                ( ( user2, keyboard ), keyboardCmd )


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

        meta' =
            { meta | eaten = True }
    in
        { food | meta = meta' }


{-| collideWithBody: collide user with another body
-}
collideWithBody : Model -> Body (Food a) -> ( Model, Body (Food a) )
collideWithBody user body =
    let
        collisionResult =
            -- TODO: only collide if we have to
            Engine.collision user body

        ( user1, body1 ) =
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
collideWithBodies : Model -> List (Body (Food a)) -> ( Model, List (Body (Food a)) )
collideWithBodies model bodies0 =
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
                ( model, [] )
                bodies0
    in
        ( user1, bodies1 )



-- VIEW


view : Model -> Form
view user =
    let
        veloLine =
            segment ( 0, 0 ) (mul2 user.velocity 5) |> traced (solid red)

        ready =
            case user.shape of
                Bubble radius ->
                    group
                        [ circle radius
                            |> filled user.color
                          -- , veloLine
                        ]

                Box extents ->
                    let
                        ( w, h ) =
                            extents
                    in
                        group
                            [ rect (w * 2) (h * 2) |> filled user.color
                            ]
    in
        Collage.move user.pos ready
