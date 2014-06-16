module Util where
import Complex (Complex, magnitude, mul, add)

iterate n f x =
    if | n == 0 -> []
       | otherwise -> x :: (iterate (n-1) f (f x))


-- check if all n numbers in [z, (f z), (f (f z)), ..] have magnitude smaller than 2
isSmallForever n z f =
    if | n == 0 -> True
       | otherwise -> if | (magnitude z) > 2 -> False
                         | otherwise -> isSmallForever (n-1) (f z) f


classify xRange yRange step f =
  let (xStart, xEnd) = xRange
      (yStart, yEnd) = yRange
      xCount = round <| (xEnd - xStart) / step
      yCount = round <| (yEnd - yStart) / step
  in
    map (\y -> map (\x -> f (Complex x y))
               (iterate xCount (\a -> a + step) xStart))
    (iterate yCount (\a -> a + step) yStart)
