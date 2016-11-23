-- Wrzesień 2013

listSum:: Int -> [[Int]]
listSum 0 = [[]]
listSum 1 = [[1]]
listSum x = let isRising []             = True
                isRising [_]            = True
                isRising (x:yys@(y:ys)) = (x < y) && isRising yys
    in [li | y <- [1..x], ys <- listSum (x-y), let li = y : ys , x == sum li, isRising li]