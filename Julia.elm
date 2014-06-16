module Julia where
import Complex (Complex, magnitude, mul, add)
import Util (..)
import Render (..)


iterations = 128
step = 0.01
const = (Complex (-1) 0)


-- for Julia fractal,
-- make changes to `const` above for different patterns.
classifySquareMinusConst c =
  isSmallForever iterations c (\x -> add (mul x x) const)


main = classify (-2,2) (-2,2) step classifySquareMinusConst
     |> renderGrid

