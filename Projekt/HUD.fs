namespace Tysiac

open System
open Common

module HUD =

///////////////////////////////////////////////////////////////////////////////
// Wyświetlanie stanu gry                                                    //
///////////////////////////////////////////////////////////////////////////////

    let private symbolKolor = function
        | Pik   -> "\u2660"
        | Trefl -> "\u2663"
        | Karo  -> "\u2666"
        | Kier  -> "\u2665"

    let private czerwonyKolor kolor aktywna = 
        match kolor with
        | Karo | Kier  -> Console.ForegroundColor <- if aktywna then  ConsoleColor.Red else ConsoleColor.DarkRed
        | Pik  | Trefl -> ()

    let private symbolWartosc = function
        | Dziewiec -> "9"
        | Walet    -> "W"
        | Dama     -> "D"
        | Krol     -> "K"
        | Dziesiec -> "10"
        | As       -> "A"

    let private pokazStol (stol: Stol) (gracze: Gracz []) (kolejnosc: Int32 list) (linijka: Int32) =
        if List.isEmpty stol.karty then printf (if linijka = 2 then "pusty" else "     ")
            else
            let dl       = (List.length stol.karty) - 1
            let poleDl   = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
            let karty    = [
                (fun _ -> printf "%-*s" poleDl "+---+")
                (fun (Karta(k,_)) -> printf "| "
                                     czerwonyKolor k true
                                     printf "%s" (symbolKolor k)
                                     Console.ForegroundColor <- ConsoleColor.Gray
                                     printf " |"
                                     if poleDl > 5 then printf "%-*s" (poleDl - 5) " ")
                (fun (Karta(_,w)) -> printf "%-*s" poleDl <| sprintf "| %-2s|" ( symbolWartosc w))
                (fun _ -> printf "%-*s" poleDl "+---+")
                           ]
            if linijka < 4 then for karta in stol.karty do karty.[linijka] karta
                           else for ind in [0 .. dl] do printf "%-*s" poleDl gracze.[kolejnosc.[ind]].imie

    let WynikRundy (gracze: Gracz []) (wyniki: Int32 []) =
        let poleDl = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie
        let poprzeczka = String.replicate poleDl "-"
        printfn "+-%s---%s---%s-+" poprzeczka poprzeczka poprzeczka
        printfn "| %*s       |" (poleDl * 3) "Wyniki rundy"
        printfn "|-%s-+-%s-+-%s-|" poprzeczka poprzeczka poprzeczka
        printfn "| %*s | %*s | %*s |" poleDl gracze.[0].imie poleDl gracze.[1].imie poleDl gracze.[2].imie 
        printfn "|-%s-+-%s-+-%s-|" poprzeczka poprzeczka poprzeczka
        printfn "| %*d | %*d | %*d |" poleDl wyniki.[0] poleDl wyniki.[1] poleDl wyniki.[2] 
        printfn "+-%s-+-%s-+-%s-+" poprzeczka poprzeczka poprzeczka
        

    let Stol (stol: Stol) (gracze: Gracz []) (kolejnosc: Int32 list) =
        for linijka in [0 .. 4] do
            pokazStol stol gracze kolejnosc linijka
            printfn ""    

    let RekaIStol (reka: Karta list) (dozw: Boolean list) (stol: Stol) (gracze: Gracz []) (kolejnosc: Int32 list) = 
        let lista = List.zip reka dozw
        let ustawKolor kol = Console.ForegroundColor <- if kol then ConsoleColor.Gray else ConsoleColor.DarkGray
        let mozeKolor = function Some k -> sprintf "%A" k | _ -> "-----"
        let stolDl = List.length stol.karty * String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
        let mutable licznik = 1
        printfn ""
        printfn "%s" <| String.replicate (3 + stolDl + 5 * List.length lista) "-"
        printfn "Kolor: %5s, Atu: %s" (mozeKolor stol.kol) (mozeKolor stol.atu)
        printfn "%-*s | Twoja ręka" stolDl "Stół"
        printfn "%4s-+-%s" (String.replicate stolDl "-") (String.replicate (5 * List.length lista) "-")
        let linijki = [   
                        (0, (fun _ _                     -> printf "+---+"))
                        (1, (fun (Karta(kol, _)) aktywna -> printf "| "; czerwonyKolor kol aktywna;  printf "%s" <| symbolKolor kol; ustawKolor aktywna; printf " |" ))
                        (2, (fun (Karta(_, war)) _       -> printf "| %-2s|" <| symbolWartosc war))
                        (3, (fun _ _                     -> printf "+---+"))
                        (4, (fun _ akt                   -> (if akt then printf "  %d  " licznik else printf "     "); licznik <- licznik + 1)) ] 
        for (ind, lam) in linijki do
            pokazStol stol gracze kolejnosc ind
            printf "| "
            for (karta, aktywna) in lista do
                ustawKolor aktywna
                lam karta aktywna
            printfn ""
            ustawKolor true

    let Reka (reka: Karta list) (liczby: Boolean) = 
        let linijki = [
            (fun _ -> printf "+---+")
            (fun (Karta(kol, _))-> printf "| "; czerwonyKolor kol true;  printf "%s" <| symbolKolor kol; Console.ForegroundColor <- ConsoleColor.Gray; printf " |")
            (fun (Karta(_, war)) -> printf "| %-2s|" <| symbolWartosc war)
            (fun _ -> printf "+---+") ]
        for linijka in linijki do
            for karta in reka do
                linijka karta
            printfn ""
        if liczby then
            for x in 1 .. List.length reka do
                printf "  %d  " x
            printfn ""

    let Historia (historia: Int32 [] list) (gracze:Gracz []) = 
        let poleDl   = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
        let poprzeczka = String.replicate (poleDl + 2) "-"
        printfn " %*s | %*s | %*s" poleDl gracze.[0].imie poleDl gracze.[1].imie poleDl gracze.[2].imie
        printfn "%s+%s+%s" poprzeczka poprzeczka poprzeczka
        for wpis in historia do
            printfn " %*d | %*d | %*d" poleDl wpis.[0] poleDl wpis.[1] poleDl wpis.[2]