namespace Tysiac

open System
open Common

module Gracz =

///////////////////////////////////////////////////////////////////////////////
// Odczyt decyzji gracza (kod imperatywny)                                   //
///////////////////////////////////////////////////////////////////////////////

    let rec Licytuj (reka: Karta list) (wynik: Int32) =
        HUD.Reka reka true
        printf "Czy chcesz przebić do %d? [t/n]: " <| wynik + 10
        let wybor = Console.ReadLine()
        if wybor = "t" then wynik + 10
            else if wybor = "n" then 0
        else Licytuj reka wynik

    let rec PodbijStawke (reka: Karta list) (wynik: Int32) =
        HUD.Reka reka false
        printf "Podaj liczbę punktów o które chcesz zagrać [wielokrotność 10, min %d]: " wynik
        let wybor =
            try Int32.Parse <| Console.ReadLine()
            with _ -> 0
        if wybor >= wynik && wybor % 10 = 0 then wybor
                                            else PodbijStawke reka wynik
    let rec ZagrajKarte (reka: Karta list) (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) =
        let wKolorze = if (stol.kol = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.kol) reka
        let wAtu     = if (stol.atu = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.atu) reka
        let dozwolone = if List.isEmpty wKolorze then if List.isEmpty wAtu then reka else wAtu 
                                                 else wKolorze
        let mozliwe = List.map (fun k -> List.contains k dozwolone) reka
        HUD.RekaIStol reka mozliwe stol gracze kolejnosc
        let wybor = 
            try Int32.Parse <| Console.ReadLine()
            with _ -> 0 in
        if wybor >= 1 && wybor <= List.length mozliwe && mozliwe.[wybor-1]
            then reka.[wybor - 1]
            else ZagrajKarte reka stol gracze kolejnosc

    let rec OddajKarte (reka: Karta list) = 
        HUD.Reka reka true
        let wybor = 
            try Int32.Parse <| Console.ReadLine()
            with _ -> 0 in
        if wybor >= 1 && wybor <= List.length reka 
            then reka.[wybor - 1]
            else OddajKarte reka