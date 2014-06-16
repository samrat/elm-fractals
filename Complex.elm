module Complex where

data Complex = Complex Float Float


realPart : Complex -> Float
realPart (Complex x _) = x


imagPart : Complex -> Float
imagPart (Complex _ y) = y
                        

mkPolar : Float -> Float -> Complex
mkPolar r theta = 
    let x = r * cos theta
        y = r * sin theta
    in Complex x y


polar : Complex -> (Float, Float)
polar (Complex x y) =
    let r = sqrt ((x^2) + (y^2))
        theta = atan2 y x
    in (r, theta)


magnitude : Complex -> Float
magnitude c =
    let (m,_) = polar c
    in m


add : Complex -> Complex -> Complex
add (Complex x y) (Complex a b) =
    Complex (x + a) (y + b)


mul : Complex -> Complex -> Complex
mul (Complex x y) (Complex a b) =
    let r = a*x - y*b
        i = x*b + y*a
    in Complex r i