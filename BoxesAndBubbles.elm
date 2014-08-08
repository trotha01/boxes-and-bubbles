module BoxesAndBubbles where

import BoxesAndBubblesEngine (..)
import Math2D (Vec2)


inf = 1/0

-- constructors

-- basic bubble with some defaults
basicBubble: Float -> Vec2 -> Vec2 -> Body
basicBubble radius pos velocity = 
  bubble radius pos velocity 1 1

-- fully specified bubble
bubble: Float -> Vec2 -> Vec2 -> Float -> Float -> Body
bubble radius pos velocity density restitution = { 
  pos = pos,
  velocity = velocity, 
  inverseMass = 1/(pi*radius*radius*density), 
  restitution = restitution,
  shape = Bubble radius
  }

box: Vec2 -> Vec2 -> Vec2 -> Float -> Float -> Body
box (w,h) pos velocity density restitution = {
  pos = pos,
  velocity = velocity,
  inverseMass = 1/(w*h*density),
  restitution = restitution,
  shape = Box (w/2,h/2)
  }

-- bounding box made up of multiple boxes with
-- arguments (width,height) center thickness
bounds: Vec2 -> Vec2 -> Float -> Float -> [Body]
bounds (w,h) (cx,cy) thickness restitution = 
  let (wExt,hExt) = (w/2,h/2)
      halfThick = thickness/2
  in [
    box (w,thickness) (cx, hExt+halfThick) (0,0) inf restitution,
    box (w,thickness) (cx, -(hExt+halfThick)) (0,0) inf restitution,
    box (thickness,h) (wExt+halfThick, cy) (0,0) inf restitution,
    box (thickness,h) (-(hExt+halfThick), cy) (0,0) inf restitution
  ]

-- updates bodies with the signal, using a fixed global force
run: Vec2 -> [Body] -> Signal a -> Signal [Body]
run gravity bodies tick = 
  let force t = (0,0)
  in foldp (step gravity) bodies (force <~ tick)
