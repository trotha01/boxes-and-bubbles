module Duplicate exposing (main)

{-| # Overview
Duplicate the user after a time period
-}

import Html.App exposing (program)
import BoxesAndBubbles.Body as Body exposing (..)
import BoxesAndBubbles exposing (..)
import BoxesAndBubbles.Math2D exposing (mul2, plus)
import List exposing (map)
import Collage exposing (..)
import Element exposing (..)
import Color exposing (..)
import AnimationFrame
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Random
import User exposing (init)
import Bodies
import Wall
import Bound


-- MODEL


type alias Model meta =
    { bodies : List (Body meta)
    , user : User.Model User.Meta
    , walls : Wall.Model Wall.Meta
    , bounds : Bound.Model Bound.Meta
    , seed : Random.Seed
    }


type alias Meta a =
    { a | isFood : Bool
    , isWall : Bool
    , isBound : Bool
    , dir : BoxesAndBubbles.Math2D.Vec2
    }

{-| TODO: get rid of this
-}
type alias SpecificMeta =
    { isFood : Bool
    , isWall : Bool
    , isBound : Bool
    , dir : BoxesAndBubbles.Math2D.Vec2
    }

initialModel : Model SpecificMeta
initialModel =
    { bodies = (someBodies meta)
    , user = User.init
    , walls = Wall.init width
    , bounds = Bound.init width height
    , seed = Random.initialSeed 3
    }


-- meta : (Meta a)
meta =
  { isFood= False
  , isWall= False
  , isBound= False
  , dir = ( 0, 0 )
  }


-- boundMeta : (Meta a)
boundMeta =
  { isFood= False
  , isWall= False
  , isBound= True
  , dir =( 0, 0 )
  }


( height, width ) =
    ( 700, 700 )
bColor : Color
bColor =
    rgb 238 130 238


boxColor : Color
boxColor =
    lightBlue


( bubbleCount, boxCount ) =
    ( 20, 10 )
randBubbles : a -> Random.Generator (List (Body a))
randBubbles meta =
    Random.list bubbleCount (randBubble bColor e0 ( -200, 200 ) ( -3, 3 ) meta)


randBoxes : a -> Random.Generator (List (Body a))
randBoxes meta =
    Random.list boxCount (randBox boxColor e0 ( -200, 200 ) ( 10, 30 ) meta)


randBody : a -> Random.Generator (Body a)
randBody meta =
    Random.bool
        `Random.andThen` (\coin ->
                            if coin then
                                randBubble bColor e0 ( -200, 200 ) ( -3, 3 ) meta
                            else
                                randBox boxColor e0 ( 10, 50 ) ( 10, 50 ) meta
                         )


{-| TODO: remove ++ bounds, since they should be separate from the bodies in the model
-}
someBodies : SpecificMeta -> List (Body (Meta SpecificMeta))
someBodies meta =
    let
        ( bubbles, seed2 ) =
            Random.step (randBubbles meta) (Random.initialSeed 2)

        ( boxes, seed3 ) =
            Random.step (randBoxes meta) seed2
    in
        bubbles
            ++
                boxes
            -- ++ bounds ( width-10, height-10) 10 e0 ( 0, 0 ) boundMeta
            ++
                bounds ( width + 300, height + 300 ) 10 e0 ( 0, 0 ) boundMeta


{-| TODO: is this even used?
-}
user : a -> Body a
user meta =
    bubble purple 100 1 e0 ( -80, 0 ) ( 1, 0 ) meta



-- VIEW


scene : ( Model meta, Keyboard.Model ) -> Element
scene ( model, keyboard ) =
    collage width height
        <| ((User.view model.user) :: (Bodies.view model.bodies) ++ (Wall.view model.walls))



-- UPDATE


type Msg
    = Tick Time
    | KeyPress Keyboard.Msg
    | BoundMsg (Bound.Msg)


update : Msg -> ( Model (Meta SpecificMeta), Keyboard.Model ) -> ( ( Model (Meta SpecificMeta), Keyboard.Model ), Cmd Msg )
update msg ( model, keyboard ) =
    case msg of
        Tick dt ->
            let
                -- update user
                ( ( user1, children, _ ), _ ) =
                    User.update (User.Tick dt) ( model.user, keyboard )

                -- collide user with the bodies
                ( user2, bodies2 ) =
                    User.collideWithBodies user1 model.bodies

                -- collide user with the wall
                user3 =
                    Wall.collideWith model.walls user2

                -- update the body collisions
                bodies3 =
                    Bodies.update (Bodies.Tick dt) bodies2

                -- collide bodies with the bounds
                ( bodies4, msgs' ) =
                    Bound.collideWithBodies model.bounds bodies3

                model2 =
                    { model | user = user3, bodies = bodies4 }

                ( ( model3, keyboard2 ), cmd2 ) =
                    List.foldl (\msg ( ( m, k ), cmd ) -> (update (BoundMsg msg) ( m, k )))
                        ( ( model2, keyboard ), Cmd.none )
                        msgs'
            in
                ( ( model3, keyboard2 ), cmd2 )

        KeyPress keyMsg ->
            let
                ( ( updatedUser, children, keyboard ), keyboardCmd ) =
                    User.update (User.KeyPress keyMsg) ( model.user, keyboard )
            in
                ( ( { model | user = updatedUser }, keyboard ), Cmd.map KeyPress keyboardCmd )

        BoundMsg msg ->
            case msg of
                Bound.Regenerate body ->
                    let
                        ( newBody, newSeed ) =
                            regenerate model.seed meta body

                        newModel =
                            { model | bodies = model.bodies ++ [ newBody ], seed = newSeed }
                    in
                        ( ( newModel, keyboard ), Cmd.none )

                _ ->
                    ( ( model, keyboard ), Cmd.none )



-- SUBSCRIPTIONS


subs : Sub Msg
subs =
    Sub.batch
        [ Sub.map KeyPress Keyboard.subscriptions
        , AnimationFrame.diffs Tick
        ]



-- MAIN


main : Program Never
main =
    let
        ( keyboard, keyboardCmd ) =
            Keyboard.init
    in
        program
            { init = ( ( initialModel, keyboard ), Cmd.map KeyPress keyboardCmd )
            , update = update
            , subscriptions = always subs
            , view = scene >> Element.toHtml
            }



-- HELPERS


inf : Float
inf =
    1 / 0


{-| e0 default restitution coefficient
-}
e0 : Float
e0 =
    0.8



-- different force functions to experiment with


noGravity t =
    ( ( 0, 0.0 ), ( 0, 0 ) )


{-| constant downward gravity
-}
constgravity t =
    ( ( 0, -0.2 ), ( 0, 0 ) )


{-| sinusoidal sideways force
-}
sinforce t =
    ( (sin <| radians (t / 1000)) * 50, 0 )


{-| small gravity, slowly accellerating upward drift
-}
counterforces t =
    ( ( 0, -0.01 ), ( 0, t / 1000 ) )


{-| regenerate is used when a body has reached the bounds
it regenerates a new body at the opposite end
-}
regenerate : Random.Seed -> a -> Body a -> ( Body a, Random.Seed )
regenerate seed meta body =
    let
        ( newBody, seed' ) =
            (Random.step (randBody meta) seed)
    in
        ( { newBody | pos = mul2 body.pos (-15 / 16), velocity = plus ( 0, 0.2 ) (mul2 body.velocity (1 / 2)) }
        , seed'
        )
