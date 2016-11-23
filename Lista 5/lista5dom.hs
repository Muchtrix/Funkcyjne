-- Wiktor Adamski

-- Zadanie 1
lrepeat :: (Int -> Int) -> [a] -> [a]
lrepeat f li =
    let aux cnt pos li = case (cnt, pos, li) of
            (_, _, [])      -> []
            (0, pos, _:xs)  -> let npos = pos + 1 in aux (f npos) npos xs
            (cnt, pos, x:_) -> x : aux (cnt-1) pos li
    in aux (f 0) 0 li

z1t1 = lrepeat (+1) [1,2,3,4,5]
z1t2 = take 30 $ lrepeat (+3) [1..]
z1t3 = lrepeat (+1) []
