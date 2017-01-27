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

    let private symbolWartosc = function
        | Dziewiec -> "9"
        | Walet    -> "W"
        | Dama     -> "D"
        | Krol     -> "K"
        | Dziesiec -> "10"
        | As       -> "A"

    let private pokazStol (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) =
        if List.isEmpty stol.karty then ["     ";"     ";"pusty";"     ";"     "]
            else
            let dl       = (List.length stol.karty) - 1
            let poleDl   = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
            let boki     = List.fold (fun aku _ -> aku + sprintf "%-*s" poleDl "+---+") "" stol.karty 
            let kolory   = List.fold (fun aku (Karta(k,_)) -> aku + (sprintf "%-*s" poleDl <| sprintf "| %s |" (symbolKolor k))) "" stol.karty
            let wartosci = List.fold (fun aku (Karta(_,w)) -> aku + (sprintf "%-*s" poleDl <| sprintf "| %-2s|" ( symbolWartosc w))) "" stol.karty
            let imiona   = List.fold (fun aku ind -> aku + (sprintf "%-*s" poleDl gracze.[kolejnosc.[ind]].imie)) "" [0 .. dl]
            [boki; kolory; wartosci; boki; imiona]

    let WynikRundy (gracze: GraczStr []) (wyniki: Int32 []) =
        let poleDl = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie
        let poprzeczka = String.replicate poleDl "-"
        printfn "+-%s---%s---%s-+" poprzeczka poprzeczka poprzeczka
        printfn "| %*s       |" (poleDl * 3) "Wyniki rundy"
        printfn "|-%s-+-%s-+-%s-|" poprzeczka poprzeczka poprzeczka
        printfn "| %*s | %*s | %*s |" poleDl gracze.[0].imie poleDl gracze.[1].imie poleDl gracze.[2].imie 
        printfn "|-%s-+-%s-+-%s-|" poprzeczka poprzeczka poprzeczka
        printfn "| %*d | %*d | %*d |" poleDl wyniki.[0] poleDl wyniki.[1] poleDl wyniki.[2] 
        printfn "+-%s-+-%s-+-%s-+" poprzeczka poprzeczka poprzeczka
        

    let Stol (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) =
        for linijka in pokazStol stol gracze kolejnosc do
            printfn "%s" linijka    

    let RekaIStol (reka: Karta list) (dozw: Boolean list) (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) = 
        let lista = List.zip reka dozw
        let kolorAktywny = Console.ForegroundColor
        let kolorNieaktywny = if (kolorAktywny = ConsoleColor.DarkGray) then ConsoleColor.DarkRed else ConsoleColor.DarkGray
        let ustawKolor kol = if kol then Console.ForegroundColor <- kolorAktywny
                                    else Console.ForegroundColor <- kolorNieaktywny
        let naStole = pokazStol stol gracze kolejnosc
        let mozeKolor = function Some k -> sprintf "%A" k | _ -> "-----"
        let mutable licznik = 1
        printfn ""
        printfn "%s" <| String.replicate (3 + String.length naStole.[0] + 5 * List.length lista) "-"
        printfn "Kolor: %5s, Atu: %s" (mozeKolor stol.kol) (mozeKolor stol.atu)
        printfn "%-*s | Twoja ręka" (String.length naStole.[0]) "Stol"
        printfn "%4s-+-%s" (String.replicate (String.length naStole.[0]) "-") (String.replicate (5 * List.length lista) "-")
        let linijki = [   
                        (0, (fun _ _               -> printf "+---+"))
                        (1, (fun (Karta(kol, _)) _ -> printf "| %s |" <| symbolKolor kol))
                        (2, (fun (Karta(_, war)) _ -> printf "| %-2s|" <| symbolWartosc war))
                        (3, (fun _ _               -> printf "+---+"))
                        (4, (fun _ akt             -> (if akt then printf "  %d  " licznik else printf "     "); licznik <- licznik + 1)) ] 
        for (ind, lam) in linijki do
            printf "%-4s | " naStole.[ind]
            for (karta, aktywna) in lista do
                ustawKolor aktywna
                lam karta aktywna
            printfn ""
            ustawKolor true

    let Reka (reka: Karta list) (liczby: Boolean) = 
        for _ in reka do
            printf "+---+"
        printfn ""
        for Karta(kol, _) in reka do
            printf "| %s |" <| symbolKolor kol
        printfn ""
        for Karta(_, war) in reka do
            printf "| %-2s|" <| symbolWartosc war
        printfn ""
        for _ in reka do
            printf "+---+"
        printfn ""
        if liczby then
            for x in 1 .. List.length reka do
                printf "  %d  " x
            printfn ""

    let Historia (historia: Int32 [] list) (gracze:GraczStr []) = 
        let poleDl   = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
        let poprzeczka = String.replicate (poleDl + 2) "-"
        printfn " %*s | %*s | %*s" poleDl gracze.[0].imie poleDl gracze.[1].imie poleDl gracze.[2].imie
        printfn "%s+%s+%s" poprzeczka poprzeczka poprzeczka
        for wpis in historia do
            printfn " %*d | %*d | %*d" poleDl wpis.[0] poleDl wpis.[1] poleDl wpis.[2]