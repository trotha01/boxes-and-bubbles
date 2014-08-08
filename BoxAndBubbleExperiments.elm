import Mouse
import BoxesAndBubblesEngine (..)
import BoxesAndBubbles (..)
import Math2D (mul2)

inf = 1/0

someBubbles = [ 
  basicBubble 30 (-80,0) (1.5,0),
  bubble 70 (80,0) (0,0) inf 1,
  basicBubble 40 (0,200) (0.4,-3.0),
  bubble 90 (300,-300) (-2,0) 0.1 0.9,
  bubble 10 (300,300) (-4,-4) 1 1,
  basicBubble 40 (200,200) (-5,-1)
  ] ++ bounds (750,750) (0,0) 15 1

someBoxes = [
--box: (w,h) pos velocity density restitution 
  box (100,100) (0,0) (0,0) 1 1,
  box (20,20) (-200,0) (3,0) 1 1,
  box (20,40) (200,200) (-1,-1) 1 1,
  box (160,160) (0,-200) (0,2) 1 1,
  box (60,60) (40,220) (-0.3,-2) 1 1
  ] ++ bounds (700,700) (0,0) 15 0.6

allIsStill = [
--box: (w,h) pos velocity density restitution 
  box (100,100) (0,0) (0,0) 1 1,
  box (20,20) (-200,0) (0,0) 1 1,
  box (20,40) (200,200) (0,0) 1 1,
  box (160,160) (0,-200) (0,0) 1 1,
  box (60,60) (40,220) (0,0) 1 1
  ] ++ bounds (700,700) (0,0) 25 0.6

someMixed = [
  bubble 30 (-200,0) (4,0) 1 1,
  box (20,40) (0,0) (0,0) 1 1
  ]

bodyInfo restitution inverseMass = 
  ["e = ", show restitution, "\nm = ", show (round (1/inverseMass))] 
  |> concat |> toText |> centered |> toForm 

drawBody {pos,velocity,inverseMass,restitution,shape} = 
  let veloLine = segment (0,0) (mul2 velocity 5) |> traced (solid red)
      info = bodyInfo restitution inverseMass
      ready = case shape of
        Bubble radius ->
          group [
            circle radius |> outlined (solid black),
            veloLine, 
            info |> move (0,radius+16)
            ]
        Box extents -> 
          let (w,h) = extents
          in group [
            rect (w*2) (h*2) |> outlined (solid black),
            veloLine, 
            info |> move (0,h+16)
          ] 
  in move pos ready  

scene bodies = 
  let drawnBodies = map drawBody bodies 
  in collage 800 800 drawnBodies

force t = ((sin <| radians (t/1000)) * 50, 0)
tick0 = force <~ foldp (+) 0 (fps 20)
--tick0 = force <~ foldp (+) 0 (fps 2)
--tick1 = force <~ count Mouse.clicks

--main = asText <~ tick0 
main = scene <~ foldp (step (0,0.05)) allIsStill tick0
--main = asText <~ foldp step someMixed tick2
