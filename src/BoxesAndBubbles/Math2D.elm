module BoxesAndBubbles.Math2D exposing (Vec2, abs2, clamp, div2, dot, getX, getY, lenSq, minus, mul, mul2, neg, norm, normalize, plus, scale, vec2)

-- just vector things


type alias Vec2 =
    ( Float, Float )


vec2 : Float -> Float -> Vec2
vec2 x y =
    ( x, y )


getX : Vec2 -> Float
getX ( x0, y0 ) =
    x0


getY : Vec2 -> Float
getY ( x0, y0 ) =
    y0


plus : Vec2 -> Vec2 -> Vec2
plus ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


minus : Vec2 -> Vec2 -> Vec2
minus ( x0, y0 ) ( x1, y1 ) =
    ( x0 - x1, y0 - y1 )



-- element-wise vector multiplication


mul : Vec2 -> Vec2 -> Vec2
mul ( x0, y0 ) ( x1, y1 ) =
    ( x0 * x1, y0 * y1 )


dot : Vec2 -> Vec2 -> Float
dot ( x0, y0 ) ( x1, y1 ) =
    x0 * x1 + y0 * y1



-- vector-scalar ops


div2 : Vec2 -> Float -> Vec2
div2 ( x, y ) a =
    ( x / a, y / a )


mul2 : Vec2 -> Float -> Vec2
mul2 ( x, y ) a =
    ( x * a, y * a )


scale : Float -> Vec2 -> Vec2
scale a v =
    mul2 v a



-- stuff


abs2 : Vec2 -> Vec2
abs2 ( x, y ) =
    ( abs x, abs y )


neg : Vec2 -> Vec2
neg ( x, y ) =
    ( -x, -y )


clamp : ( Float, Float ) -> ( Float, Float ) -> Vec2 -> Vec2
clamp ( xMin, xMax ) ( yMin, yMax ) ( x, y ) =
    ( Basics.clamp xMin xMax x, Basics.clamp yMin yMax y )



-- squared norm/length of vector


lenSq : Vec2 -> Float
lenSq ( x, y ) =
    x * x + y * y


norm : Vec2 -> Vec2
norm v =
    div2 v <| sqrt (lenSq v)


normalize : Vec2 -> Vec2
normalize =
    norm
