-- Wiktor Adamski
import Control.Monad.State

data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Eq, Ord, Show)

-- Zadanie 1
label:: Tree a -> Tree (a, Int)
label tr = 
    let aux (Leaf x) lab = (Leaf(x, lab), lab + 1)
        aux (Branch tl tr) lab = 
            let (ltl, labl) = aux tl lab
                (ltr, labr) = aux tr labl
            in (Branch ltl ltr, labr)
    in fst $ aux tr 0

test = let t = Branch (Leaf 'a') (Leaf 'b')
    in label $ Branch t t

-- Zadanie 2
mlabel:: Tree a -> Tree (a, Int)
mlabel tr = 
    let mlabelC (Leaf x) = do
            v <- get
            put $ v + 1
            return $ Leaf (x, v)
        mlabelC (Branch tl tr) = do
            ltl <- mlabelC tl
            ltr <- mlabelC tr
            return $ Branch ltl ltr
    in evalState (mlabelC tr) 0
    
test2 = let t = Branch (Leaf 'a') (Leaf 'b')
    in mlabel $ Branch t t