module Julia where
import Complex (Complex, magnitude, mul, add)


iterations = 128
step = 0.01
const = (Complex (-1) 0)


iterate n f x =
    if | n == 0 -> []
       | otherwise -> x :: (iterate (n-1) f (f x))


-- check if all n numbers in [z, (f z), (f (f z)), ..] have magnitude smaller than 2
isSmallForever n z f =
    if | n == 0 -> True
       | otherwise -> if | (magnitude z) > 2 -> False
                         | otherwise -> isSmallForever (n-1) (f z) f


-- for Julia fractal,
-- make changes to `const` above for different patterns.
classifySquareMinusConst c =
  isSmallForever iterations c (\x -> add (mul x x) const)


-- for Mandelbrot fractal
classifySquareMinusC c =
  isSmallForever iterations (Complex 0 0) (\x -> add (mul x x) c)


classify xRange yRange step f =
  let (xStart, xEnd) = xRange
      (yStart, yEnd) = yRange
      xCount = round <| (xEnd - xStart) / step
      yCount = round <| (yEnd - yStart) / step
  in
    map (\y -> map (\x -> f (Complex x y))
               (iterate xCount (\a -> a + step) xStart))
    (iterate yCount (\a -> a + step) yStart)
   

-- Rendering
renderCell : Bool -> Element
renderCell on =
    spacer 1 1
           |> color (if on then (rgb 0 0 0) else (rgb 255 255 255))


renderRow : [Bool] -> [Element]
renderRow = map renderCell


renderGrid : [[Bool]] -> Element
renderGrid grid =
    grid
        |> map renderRow
        |> map (flow right)
        |> flow down
        |> container 600 600 topLeft


main = classify (-2,2) (-2,2) step classifySquareMinusConst
     |> renderGrid

-- main = classify (-2,2) (-2,2) step classifySquareMinusC
--      |> renderGrid
