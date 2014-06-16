module Mandelbrot where
import Complex (Complex, magnitude, mul, add)
import Util (..)
import Render (..)

iterations = 128
step = 0.01


classifySquareMinusC c =
  isSmallForever iterations (Complex 0 0) (\x -> add (mul x x) c)


main = classify (-2,2) (-2,2) step classifySquareMinusC
     |> renderGrid
