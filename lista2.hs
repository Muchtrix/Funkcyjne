-- Wiktor Adamski

-- Zadanie 6
repeatList :: [Int] -> [Int]
repeatList [] = []
repeatList (x:xs) = (take x $ repeat x) ++ (repeatList xs)

z6t1 = repeatList [1,0,4,3]
z6t2 = repeatList []
