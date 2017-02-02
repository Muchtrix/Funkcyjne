namespace Tysiac

open System
open Common

module Czlowiek =

///////////////////////////////////////////////////////////////////////////////
// Odczyt decyzji gracza (kod imperatywny)                                   //
///////////////////////////////////////////////////////////////////////////////

    type Czlowiek () =
        inherit RodzajGracza ()
        static member private WczytajKarte (linijka: String) =
            let wczytajWartosc (war: String) = 
                match war.ToLower () with
                | "9"     -> Some Wartosc.Dziewiec
                | "walet" -> Some Wartosc.Walet
                | "dama"  -> Some Wartosc.Dama
                | "król"  -> Some Wartosc.Krol
                | "10"    -> Some Wartosc.Dziesiec
                | "as"    -> Some Wartosc.As
                | _       -> None
            let wczytajKolor (kol: String) =
                match kol.ToLower () with
                | "pik"   -> Some Kolor.Pik
                | "trefl" -> Some Kolor.Trefl
                | "karo"  -> Some Kolor.Karo
                | "kier"  -> Some Kolor.Kier
                | _       -> None
            let slowa = linijka.Split ' '
            if Array.length slowa <> 2 then None
                else match (wczytajWartosc slowa.[0], wczytajKolor slowa.[1]) with
                     | (Some w, Some k) -> Some (Karta(k, w))
                     | _                -> None

        override this.Licytuj (reka: Karta list) (wynik: Int32) =
            HUD.Reka reka true
            printf "Czy chcesz przebić do %d? [T/n]: " <| wynik + 10
            match (Console.ReadLine()).ToLower () with
            | "t" | "" -> wynik + 10
            | "n"      -> 0
            | _        -> this.Licytuj reka wynik

        override this.PodbijStawke (reka: Karta list) (wynik: Int32) =
            HUD.Reka reka false
            printf "Podaj liczbę punktów o które chcesz zagrać [wielokrotność 10, domyślnie %d]: " wynik
            let linijka = Console.ReadLine()
            if linijka = "" then wynik
            else
            let wybor = try Int32.Parse linijka with _ -> 0
            if wybor >= wynik && wybor % 10 = 0 then wybor else this.PodbijStawke reka wynik

        override this.ZagrajKarte (reka: Karta list) (stol: Stol) (gracze: Gracz []) (kolejnosc: Int32 list) =
            let wKolorze  = if (stol.kol = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.kol) reka
            let wAtu      = if (stol.atu = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.atu) reka
            let dozwolone = if List.isEmpty wKolorze then if List.isEmpty wAtu then reka else wAtu 
                                                    else wKolorze
            let mozliwe = List.map (fun k -> List.contains k dozwolone) reka
            HUD.RekaIStol reka mozliwe stol gracze kolejnosc
            printf "Podaj którą kartę chcesz zagrać: "
            let linijka = Console.ReadLine()
            match Czlowiek.WczytajKarte linijka with
            | Some k  when List.contains k dozwolone -> k
            | _                                      -> let wybor = try Int32.Parse linijka with _ -> 0
                                                        if wybor >= 1 && wybor <= List.length mozliwe && mozliwe.[wybor-1]
                                                        then reka.[wybor - 1]
                                                        else this.ZagrajKarte reka stol gracze kolejnosc

        override this.OddajKarte (reka: Karta list) (imie: String) = 
            printfn "Wybierasz kartę dla %s" <| imie + "a"
            HUD.Reka reka true
            printf "Podaj którą kartę chcesz oddać: "
            let linijka = Console.ReadLine()
            match Czlowiek.WczytajKarte linijka with
            | Some k  when List.contains k reka -> k
            | _                                 -> let wybor = try Int32.Parse linijka with _ -> 0 
                                                   try reka.[wybor-1] with _ -> this.OddajKarte reka imie