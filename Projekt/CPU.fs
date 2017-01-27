namespace Tysiac

open System
open Common

module CPU =

///////////////////////////////////////////////////////////////////////////////
// Gracze CPU                                                                //
///////////////////////////////////////////////////////////////////////////////

    let private pewnyKolor (reka: Karta list) = 
        let ileKart = List.length reka
        let ileOdGory = 
            let wartosci = [As; Dziesiec; Krol; Dama; Walet; Dziewiec]
            List.zip (List.sortByDescending (fun (Karta(_,w)) -> w)reka) (List.take ileKart wartosci)
                |> List.takeWhile (fun (Karta(_,w), war) -> w = war)
                |> List.length
        ileKart + ileOdGory >= 6

    let private pewnePunkty (reka: Karta list) =
        let ileKart = List.length reka
        let wartosci = [As; Dziesiec; Krol; Dama; Walet; Dziewiec]
        List.zip (List.sortByDescending (fun (Karta(_,w)) -> w)reka) (List.take ileKart wartosci)
                |> List.takeWhile (fun (Karta(_,w), war) -> w = war)
                |> List.map  (punktyZaKarte << fst)
                |> List.sum

    let private podzielNaKolory (reka: Karta list) =
        [Pik; Trefl; Karo; Kier]
            |> List.map (fun k -> (List.filter (fun (Karta(ko, _)) -> ko = k) reka, k)) 

    let private sprawdzMeldunki (reka: Karta list) = 
        podzielNaKolory reka
            |> List.filter (fun (karty, kolor)-> znajdzMeldunek kolor karty)
            |> List.map snd
            |> List.rev

    let Licytuj (reka: Karta list) (wynik: Int32) = 
        let przewidywana = (wynikLewy reka) + 70
        if przewidywana >= wynik + 10 then wynik + 10
                                    else 0

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

    let OddajKarte (reka: Karta list) =
        (List.sortBy (fun (Karta(_, w)) -> w) reka).[0]

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