-- Wiktor Adamski
-- Lista 8

module Main(main) where
import System.Random

main:: IO ()
main = do
       putStrLn "===Gra Nim==="
       len <- randomRIO(3,6)
       plansza <- losowaPlansza len
       drukujPlansze plansza 
       nim plansza

losowaPlansza:: Int -> IO [Int]
losowaPlansza 0 = return []
losowaPlansza len = do
                  x <- randomRIO(1, 10)
                  xs <- losowaPlansza $ len - 1
                  return $ x:xs

nim:: [Int] -> IO ()
nim plansza = do
              putStrLn "Runda Gracza"
              nowa <- ruchGracza plansza
              drukujPlansze nowa
              if koniecGry nowa 
                  then putStrLn "Wygrana!"
                  else do
                       putStrLn "Runda Komputera"
                       komp <- ruchKomputera nowa
                       drukujPlansze komp 
                       if koniecGry komp
                           then putStrLn "Przegrana"
                           else nim komp

ruchGracza:: [Int] -> IO [Int]
ruchGracza plansza = do
                     let dostepne = filter (\x -> plansza !! (x - 1) /= 0) [1.. (length plansza)]
                     stos <- wczytajLiczbe dostepne "Podaj numer stosu:"
                     if stos `notElem` dostepne 
                         then ruchGracza plansza
                         else do
                              let maxKamienie = plansza !! (stos - 1)
                              kamienie <- wczytajLiczbe [1..maxKamienie] "Podaj liczbę kamieni:"
                              return $ nowaPlansza plansza (stos - 1) (maxKamienie - kamienie)

ruchKomputera:: [Int] -> IO [Int]
ruchKomputera plansza = do
                        stos <- randomRIO(1,length plansza)
                        let maxKamienie = plansza !! (stos - 1)
                        if maxKamienie == 0 
                            then ruchKomputera plansza
                            else do 
                                 kamienie <- randomRIO (1, maxKamienie)
                                 putStrLn $ "Komputer zabrał " ++ show kamienie ++ " " ++ odmianaKamienia kamienie ++ " ze stosu nr " ++ show stos
                                 return $ nowaPlansza plansza (stos - 1) (maxKamienie - kamienie)

nowaPlansza:: [Int] -> Int -> Int -> [Int]
nowaPlansza pl st il = a ++ [il] ++ tail b where (a, b) = splitAt st pl

koniecGry:: [Int] -> Bool
koniecGry = all (==0)

drukujStos:: Int -> IO ()
drukujStos 0 = return ()
drukujStos x = do 
               putStr "*"
               drukujStos $ x - 1

drukujPlansze:: [Int] -> IO ()
drukujPlansze plansza = 
    let aux [] _ = return ()
        aux (x:xs) cnt = do
                         putStr $ show cnt ++ ". "
                         drukujStos x
                         putStrLn ""
                         aux xs (cnt + 1)
    in do
       putStrLn ""
       aux plansza 1
       putStrLn ""

wczytajLiczbe:: [Int] -> String -> IO Int
wczytajLiczbe avail msg = do
                        putStrLn $ msg ++ " " ++ show avail
                        linijka <- getLine
                        let num = read linijka
                        if num `elem` avail 
                            then return num 
                            else wczytajLiczbe avail msg

odmianaKamienia:: Int -> String
odmianaKamienia x
    | x == 1          = "kamień"
    | x `elem` [2..4] = "kamienie"
    | otherwise       = "kamieni"