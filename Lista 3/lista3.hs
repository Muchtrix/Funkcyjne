-- Wiktor Adamski

-- Zadanie 4
mergesort :: (a->a->Bool)->[a]->[a]
mergesort _ []    = []
mergesort _ [x]   = [x]
mergesort f [x,y] = if f x y then [x,y] else [y,x]
mergesort f li    = 
    let merge _ [] [] = []
        merge _ xs [] = xs
        merge _ [] ys = ys
        merge f (x:xs) (y:ys) = if f x y then x : (merge f xs (y:ys)) else y:(merge f (x:xs) ys)
    in merge f (mergesort f (take len li)) (mergesort f (drop len li))
    where len = (length li) `div` 2

z4t1 = mergesort (<=) [9,8,7,6,5,4,3,2,1]

-- Test sprawdzajacy stabilnosc
z4t2 = mergesort (\x y -> fst x <= fst y) [(1,4), (1,2), (3,6), (0,5)]