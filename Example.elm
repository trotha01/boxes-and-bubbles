import BoxesAndBubblesBodies (..)
import BoxesAndBubbles (..)
import Math2D (mul2)

inf = 1/0
e0 = 0.8

--box: (w,h) pos velocity density restitution 
--bubble: radius pos velocity density restitution

someBodies = [ 
  bubble 30 1 e0(-80,0) (1.5,0),
  bubble 70 inf 0 (80,0) (0,0) ,
  bubble 40 1 e0 (0,200) (0.4,-3.0),
  bubble 80 0.1 e0 (300,-280) (-2,1),
  bubble 15 5 0.4 (300,300) (-4,-3),
  bubble 40 1 e0 (200,200) (-5,-1),
  box (100,100) 1 e0 (300,0) (0,0),
  box (20,20) 1 e0 (-200,0) (3,0),
  box (20,40) 1 e0 (200,-200) (-1,-1)
  ] ++ bounds (750,750) 100 e0 (0,0)

-- extending bodies with an arbitrary string label
type Labeled = { label: String }

-- we'll just compute the label from the data in the body
bodyLabel restitution inverseMass = 
  ["e = ", show restitution, "\nm = ", show (round (1/inverseMass))] |> concat

-- and attach it to all the bodies
labeledBodies = map (\b -> { b | label = bodyLabel b.restitution b.inverseMass }) someBodies

-- why yes, it draws a body with label. Or creates the Element rather
drawBody {pos,velocity,inverseMass,restitution,shape,label} = 
  let veloLine = segment (0,0) (mul2 velocity 5) |> traced (solid red)
      info = label |> toText |> centered |> toForm 

      ready = case shape of
        Bubble radius ->
          group [ 
            circle radius |> outlined (solid black),
            info |> move (0,radius+16),
            veloLine
            ]
        Box extents -> 
          let (w,h) = extents
          in group [
            rect (w*2) (h*2) |> outlined (solid black),
            info |> move (0,h+16),
            veloLine            
          ] 
  in move pos ready  

scene bodies = collage 800 800 <| map drawBody bodies 

constgravity t = ((0,-0.2), (0,0)) -- constant downward gravity
sinforce t = ((sin <| radians (t/1000)) * 50, 0) -- sinuoidal sideways force
counterforces t = ((0,-0.01), (0, t/1000)) -- slowly accellerating upward drift

tick = counterforces <~ foldp (+) 0 (fps 40)

main = scene <~ run labeledBodies tick
