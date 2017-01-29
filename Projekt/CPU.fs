namespace Tysiac

open System
open Common

module CPU =

///////////////////////////////////////////////////////////////////////////////
// Gracze CPU                                                                //
///////////////////////////////////////////////////////////////////////////////

    let private podzielNaKolory (reka: Karta list) =
        [Kier; Karo; Trefl; Pik]
            |> List.map (fun k -> (List.filter (fun (Karta(ko, _)) -> ko = k) reka, k)) 
    
    let private informacjeOKolorze (reka: Karta list, kolor: Kolor) = 
        if List.isEmpty reka then (0, false,  [], [], false, 0)
            else
            let ileKart = List.length reka
            let kartyOdGory = 
                let wartosci = [As; Dziesiec; Krol; Dama; Walet; Dziewiec]
                List.zip (List.sortByDescending (fun (Karta(_,w)) -> w)reka) (List.take ileKart wartosci)
                    |> List.takeWhile (fun (Karta(_,w), war) -> w = war)
                    |> List.map fst
            let ileOdGory = List.length kartyOdGory
            let calyKolor = ileOdGory + ileKart >= 6
            let kartyPozostale = List.fold usunZReki reka kartyOdGory
            let pewnyMeldunek = znajdzMeldunek kolor kartyOdGory
            let wartMeldunku = if znajdzMeldunek kolor reka then punktyZaMeldunek kolor else 0
            (ileOdGory, calyKolor, kartyOdGory, kartyPozostale, pewnyMeldunek, wartMeldunku)

    let private oszacujPunkty (reka: Karta list) =
        let wartosciKart = [11; 11; 11; 11; 10; 10; 10; 10; 4; 4; 4; 4; 3; 3; 3; 3; 2; 2; 2; 2; 0; 0; 0; 0]
        let rec usunPierwszy lista el = 
            match lista with
            | []    -> []
            | x::xs -> if x = el then xs else x :: usunPierwszy xs el
        let infoOKolorach = podzielNaKolory reka |> List.map informacjeOKolorze
        let zMeldunkow = fst <| List.fold 
                                (fun (suma, pewne) (_, _, _, _, pMeld, wMeld) -> if wMeld = 0 || (not pMeld && not pewne) then (suma, pewne) else (suma + wMeld, pMeld))                     
                                (0, true) infoOKolorach
        let niezebraneLewy = 8 - List.sumBy (fun (x, _, _, _, _, _) -> x) infoOKolorach
        let zebraneWartosci = [for (_, caly, karty, _, _, _) in infoOKolorach do yield! if caly then [0; 2; 3; 4; 10; 11] else List.map punktyZaKarte karty ]
        let pozostaleWartosci = [for (_, _, _, karty, _, _) in infoOKolorach do yield! List.map punktyZaKarte karty]
        let wartosciPrzeciwnikow = List.fold usunPierwszy (List.fold usunPierwszy wartosciKart zebraneWartosci) pozostaleWartosci
        120 + zMeldunkow - List.sum (List.take (2 * niezebraneLewy) wartosciPrzeciwnikow)

    let private sprawdzMeldunki (reka: Karta list) = 
        podzielNaKolory reka
            |> List.filter (fun (karty, kolor) -> znajdzMeldunek kolor karty)
            |> List.map snd

    let Licytuj (reka: Karta list) (wynik: Int32) = 
        let przewidywana = ((oszacujPunkty reka) / 10) * 10
        if przewidywana >= wynik + 10 then wynik + 10
                                    else 0
    
    let PodbijStawke (reka: Karta list) (wynik: Int32) =
        let przewidywana = ((oszacujPunkty reka) / 10) * 10
        if przewidywana > wynik then przewidywana else wynik

    let OddajKarte (reka: Karta list) =
        (List.sortBy (fun (Karta(_, w)) -> w) reka).[0]

    let private wygrywajaceKarty (reka: Karta list) (stol: Stol) =
        let pozostale = (Set.ofList stol.pozostale) - (Set.ofList reka)
        let kombinacje k =
            let (Karta(kk,_)) = k 
            match stol.karty with
            | [Karta(kol,_) as k1; k2] -> [([k; k1; k2], kol)]
            | [Karta(kol,_) as k1]     -> [for k2 in pozostale do if k1 <> k2 then yield ([k; k1; k2], kol)]
            | _        -> [for k1 in pozostale do for k2 in pozostale do if k1 <> k2 then yield ([k; k1; k2], kk)]
        let wygrywa k = List.forall id <| List.map (fun (komb, kol) -> 0 = zwyciezcaLewy {stol with karty = komb; pozostale = []; kol = Some kol} ) (kombinacje k) 
                            
        let wyg = List.filter wygrywa reka |> List.sortBy (fun (Karta(_,w)) -> w)
        let prz = List.filter (not << wygrywa) reka |> List.sortBy (fun (Karta(_,w)) -> w)
        (wyg,prz)

    let ZagrajKarte (reka: Karta list) (stol : Stol) =
        let wKolorze = if (stol.kol = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.kol) reka
        let wAtu     = if (stol.atu = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.atu) reka
        let dozwolone = if List.isEmpty wKolorze then if List.isEmpty wAtu then reka 
                                                                        else wAtu 
                                                else wKolorze
        let meldunki = sprawdzMeldunki dozwolone
        let mozeMeldowac = List.isEmpty stol.karty && not <| List.isEmpty meldunki
        match wygrywajaceKarty dozwolone stol with
        | (x::_, _)                 -> x
        | ([], _) when mozeMeldowac -> Karta(meldunki.[0], Dama)
        | ([], x::_)                -> x
        | _                         -> failwith "impossible"