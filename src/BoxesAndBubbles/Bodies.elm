module BoxesAndBubbles.Bodies exposing (Body, Shape(..), move)

{-| # Boxes and Bubbles Bodies.
Defines bodies as used by the Boxes and Bubbles engine. You will need these data types to
display and modify bodies being calculated. For creating them, you may prefer the constructor
functions in the BoxesAndBubbles module.

@docs Body, Shape

-}

import BoxesAndBubbles.Math2D exposing (Vec2)


{-| A rigid body in the Boxes and Bubbles universe, as used internally by the engine.
Mass is stored as inverse, because it is more convenient for calculation.

Type parameter `meta` can be used to attach arbitrary other information used
by your application to bodies. For example: label, hit points, an object type ADT, or more low-level,
an id used to associate the body with arbitrary other data via a Dict.
-}
type alias Body meta =
    { pos : Vec2
    , -- reference position (center)
      velocity : Vec2
    , -- direction and speed
      inverseMass : Float
    , -- we usually use only inverse mass for calculations
      restitution : Float
    , -- bounciness factor
      shape : Shape
    , meta : meta
    }


{-| Shape data for a body.
A bubble is defined by its radius.
A box is defined by its extents (half-width/half-height from the center).
We use half-lengths because that's what is convenient for calculation, and it's most consistent
with using radius for circles.
-}
type Shape
    = Box Vec2
      -- vector of extents (half-widths)
    | Bubble Float


type alias Dir =
    { x : Int
    , y : Int
    }


move : Body meta -> Dir -> Body meta
move body dir =
    let
        ( x, y ) =
            body.pos

        ( vX, vY ) =
            body.velocity

        newPos =
            ( x + (toFloat dir.x), y + (toFloat dir.y) )

        newVel =
            ( clamp -4 4 (vX + (toFloat dir.x) / 2), clamp -4 4 (vY + (toFloat dir.y) / 2) )
    in
        { body | pos = newPos, velocity = newVel }



-- radius