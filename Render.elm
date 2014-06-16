module Render where 

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
