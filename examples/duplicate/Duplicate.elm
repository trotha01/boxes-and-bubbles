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
import Text
import Random
import User exposing (init)
import Bodies
import Wall
import Bound


-- MODEL


type alias Model meta =
    { bodies : List (Body meta)
    , user : User.Model User.Meta
    , children : List (Body meta)
    , walls : Wall.Model Wall.Meta
    , bounds : Bound.Model Bound.Meta
    , seed : Random.Seed
    , points : Int
    }


type alias Meta a =
    { a | isFood : Bool
    , eaten : Bool
    , isWall : Bool
    , isBound : Bool
    , dir : BoxesAndBubbles.Math2D.Vec2
    }

{-| TODO: get rid of this
-}
type alias SpecificMeta =
    { isFood : Bool
    , eaten : Bool
    , isWall : Bool
    , isBound : Bool
    , dir : BoxesAndBubbles.Math2D.Vec2
    }

initialModel : Model SpecificMeta
initialModel =
    { bodies = (someBodies meta)
    , user = User.init
    , children = []
    , walls = Wall.init width
    , bounds = Bound.init width height
    , seed = Random.initialSeed 3
    , points = 0
    }


-- meta : (Meta a)
meta =
  { isFood= False
  , eaten = False
  , isWall= False
  , isBound= False
  , dir = ( 0, 0 )
  }

food =
  { isFood= True
  , eaten = False
  , isWall= False
  , isBound= False
  , dir = ( 0, 0 )
  }


( height, width ) =
    ( 700, 700 )
( halfHeight, halfWidth ) =
    ( height/2, width/2)
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


randBody : Random.Generator (Body SpecificMeta)
randBody =
    Random.bool
        `Random.andThen` (\coin ->
                            if coin then
                                randBubble bColor e0 ( -200, 200 ) ( -3, 3 ) food
                            else
                                randBox boxColor e0 ( 10, 50 ) ( 10, 50 ) meta
                         )


someBodies : SpecificMeta -> List (Body (Meta SpecificMeta))
someBodies meta =
    let
        ( bubbles, seed2 ) =
            Random.step (randBubbles food) (Random.initialSeed 2)

        ( boxes, seed3 ) =
            Random.step (randBoxes meta) seed2
    in
        (bubbles ++ boxes)



-- VIEW


scene : ( Model meta, Keyboard.Model ) -> Element
scene ( model, keyboard ) =
    collage width height
        <| ((User.view model.user) :: (Bodies.view model.bodies ++ Bodies.view model.children)
           ++ (Wall.view model.walls))
           ++ points model

points : Model meta -> List Form
points model =
      [(text (Text.fromString (toString model.points))) |> Collage.move (halfWidth-50, halfHeight-50) ]


-- UPDATE


type Msg
    = Tick Time
    | Points Int
    | KeyPress Keyboard.Msg
    | BoundMsg (Bound.Msg SpecificMeta)


update : Msg -> ( Model (Meta SpecificMeta), Keyboard.Model ) -> ( ( Model (Meta SpecificMeta), Keyboard.Model ), Cmd Msg )
update msg ( model, keyboard ) =
    case msg of
        Points p ->
            let ((_, children, _), _) = User.update User.MakeChild (model.user, keyboard)
                model2 = {model|points = model.points + p}
                model3 =
                  if model2.points /= 0 && model2.points % 100 == 0
                  then {model2 | children = model2.children ++ children}
                  else model2
            in ((model3, keyboard), Cmd.none)
        Tick dt ->
            let
                -- update user
                ( ( user1, children, _ ), _ ) =
                    User.update (User.Tick dt) ( model.user, keyboard )

                -- collide user with the bodies
                ( user2, bodies2 ) =
                    User.collideWithBodies user1 model.bodies

                -- collide user with the children
                -- ( user3, children2) =
                --     User.collideWithBodies user1 (model.children ++ children)

                -- collide user with the wall
                user4 =
                    Wall.collideWith model.walls user2

                -- update the body collisions
                (bodies3, pointMsgs) =
                    Bodies.update (Bodies.Tick dt) (bodies2)

                -- update the children collisions
                (children3, _) =
                    Bodies.update (Bodies.Tick dt) (model.children ++ children)

                -- collide bodies with the bounds
                ( bodies4, msgs' ) =
                    Bound.collideWithBodies model.bounds bodies3

                model2 =
                    { model | user = user4, bodies = bodies4, children = children3 }

                -- hack, since I don't know how to generate a Cmd
                ( ( model3, keyboard2 ), cmd2 ) =
                    List.foldl (\msg ( ( m, k ), cmd ) -> (update (BoundMsg msg) ( m, k )))
                        ( ( model2, keyboard ), Cmd.none )
                        msgs'

                -- hack2, since I don't know how to generate a Cmd
                ( ( model4, keyboard3 ), cmd3 ) =
                    List.foldl
                        (\msg ( ( m, k ), cmd ) ->
                          case msg of
                              (Bodies.Points p) -> (update (Points p) (m, k))
                              _ -> ((m, k), cmd))
                        ( ( model3, keyboard2 ), Cmd.none )
                        pointMsgs
            in
                ( ( model4, keyboard3 ), cmd3 )

        KeyPress keyMsg ->
            let
                ( ( updatedUser, _, keyboard ), keyboardCmd ) =
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
regenerate : Random.Seed -> SpecificMeta -> Body SpecificMeta -> ( Body SpecificMeta, Random.Seed )
regenerate seed meta body =
    let
        ( newBody, seed' ) =
            (Random.step (randBody) seed)
    in
        ( { newBody | pos = mul2 body.pos (-15 / 16), velocity = plus ( 0, 0.2 ) (mul2 body.velocity (1 / 2)) }
        , seed'
        )