-- Wiktor Adamski

data BT a = Empty | Node a (BT a) (BT a)
     deriving (Eq, Ord, Show, Read)

tr1 = Empty
tr2 = Node 14 Empty Empty
tr3 = Node 4 (Node 1 Empty Empty) (Node 5 (Node 3 Empty Empty) Empty)

-- Zadanie 3
sumBT Empty        = 0
sumBT (Node x y z) = x + sumBT y + sumBT z

z3t1 = sumBT tr1
z3t2 = sumBT tr2
z3t3 = sumBT tr3

-- Zadanie 4
foldBT :: (a->(b,b)->b)->b->BT a->b
foldBT _ acc Empty          = acc
foldBT f acc (Node x lt rt) = f x (foldBT f acc lt, foldBT f acc rt)

-- Zadanie 5
sumBT' = foldBT (\x (y,z) -> x + y + z) 0
listBT' = foldBT (\x (y,z) -> x:y ++ z) []

z5t1 = sumBT' tr1
z5t2 = sumBT' tr2
z5t3 = sumBT' tr3

z5t4 = listBT' tr1
z5t5 = listBT' tr2
z5t6 = listBT' tr3

-- Zadanie 6
mapBT :: (a->b)->BT a->BT b
mapBT f = foldBT (\x (y,z)-> (Node (f x) y z)) Empty

z6t1 = mapBT (+1) tr1
z6t2 = mapBT (+1) tr2
z6t3 = mapBT (+1) tr3
