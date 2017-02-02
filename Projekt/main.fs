open System
open System.Runtime.InteropServices
open Tysiac
open Tysiac.Common
open Tysiac.CPU
open Tysiac.Czlowiek

///////////////////////////////////////////////////////////////////////////////
// Funkcja systemowa umożliwająca odpowiednie wyświetlanie znaków            //
///////////////////////////////////////////////////////////////////////////////
module Kernel =
    [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern bool SetConsoleOutputCP(uint32 wCodePageID)

///////////////////////////////////////////////////////////////////////////////
// Przebieg rozrywki (mnóstwo kodu imperatywnego)                            //
///////////////////////////////////////////////////////////////////////////////
let licytacja (gracze: Gracz []) (musik: Karta list) (rozpoczynajacy: Int32) =
    let mutable akt  = rozpoczynajacy
    let mutable zwyc = rozpoczynajacy
    let mutable maks = 100
    let lic = [|true; true; true|]
    let mutable pozostali = 3
    while pozostali > 1 do
        akt <- (akt + 1) % 3
        while not lic.[akt] do
            akt <- (akt + 1) % 3
        let stawka = gracze.[akt].rodzaj.Licytuj gracze.[akt].karty maks
        if 0 = stawka then lic.[akt] <- false
                           printfn "%s pasuje" gracze.[akt].imie
                           pozostali <- pozostali - 1
                      else printfn "%s podbija do %d" gracze.[akt].imie stawka
                           zwyc <- akt
                           maks <- stawka
    printfn "%s wygrywa licytację z wynikiem %d" gracze.[zwyc].imie maks
    printfn "Musik:"
    HUD.Reka musik false
    gracze.[zwyc].karty <- List.fold dodajDoReki gracze.[zwyc].karty musik
    for i in [1; 2] do
        let oddana = gracze.[zwyc].rodzaj.OddajKarte gracze.[zwyc].karty gracze.[i].imie
        gracze.[zwyc].karty <- usunZReki gracze.[zwyc].karty oddana
        gracze.[(zwyc + i) % 3].karty <- dodajDoReki gracze.[(zwyc + i) % 3].karty oddana
    maks <- gracze.[zwyc].rodzaj.PodbijStawke gracze.[zwyc].karty maks
    (zwyc, maks)

let rozgrywka (pocz: Int32) (gracze: Gracz []) = 
    let stol = {karty = []; atu = None; kol = None; pozostale = talia}
    let mutable kolejnosc = [pocz; (pocz + 1) % 3; (pocz + 2) % 3]
    for _ in 1 .. 8 do
        stol.karty <- []
        stol.kol <- None
        for i in kolejnosc do
            let g = gracze.[i]
            let Karta(k, w) as zagrana = g.rodzaj.ZagrajKarte g.karty stol gracze kolejnosc
            if List.isEmpty stol.karty then                          // Wybór atu (pierwszy gracz w lewie)
                stol.kol <- Some k
                if (w = Krol || w = Dama) && znajdzMeldunek k g.karty then // Sprawdzenie meldunku
                    stol.atu <- Some k
                    let meld = punktyZaMeldunek k
                    g.wynik <- g.wynik + meld
                    printfn "%s melduje za %d punktów!" g.imie meld
            stol.pozostale <- usunZReki stol.pozostale zagrana
            g.karty        <- usunZReki g.karty zagrana
            stol.karty     <- stol.karty @ [zagrana]
        HUD.Stol stol gracze kolejnosc
        let punkty = wynikLewy stol.karty
        let zwyc   = kolejnosc.[zwyciezcaLewy stol]
        gracze.[zwyc].wynik <- gracze.[zwyc].wynik + punkty
        printfn "Lewę wartą %d pkt. zdobywa %s" punkty gracze.[zwyc].imie
        kolejnosc <- [zwyc; (zwyc + 1) % 3; (zwyc + 2) % 3]
    HUD.WynikRundy gracze [|for g in gracze do yield g.wynik|]
    [|gracze.[0].wynik; gracze.[1].wynik; gracze.[2].wynik|]

let runda (graczeInfo: Gracz []) (rozpoczynajacy: Int32) =
    let (r1, r2, r3, musik) = rozdaj()
    let gracze = [|
        {graczeInfo.[0] with wynik = 0; karty = r1}
        {graczeInfo.[1] with wynik = 0; karty = r2}
        {graczeInfo.[2] with wynik = 0; karty = r3}
        |]
    printfn ""
    printfn "--- Licytacja ---"
    printfn ""
    let (zwyc, maks) = licytacja gracze musik rozpoczynajacy
    printfn ""
    printfn "--- Rozgrywka ---"
    printfn ""
    let wyniki = rozgrywka zwyc gracze
    if maks > wyniki.[zwyc] then printfn "%s nie ugrał i traci %d punktów" gracze.[zwyc].imie maks
                                 wyniki.[zwyc] <- -1 * maks
                            else wyniki.[zwyc] <- maks
    Array.map (fun x -> if x > 0 then ((x+5)/10)*10 else x) wyniki

[<EntryPoint>]
let gra args =
    Kernel.SetConsoleOutputCP 65001u |> ignore // Przestawienie konsoli w tryb UTF-8
    let kolorKonsoli = Console.ForegroundColor
    Console.ForegroundColor <- ConsoleColor.Gray
    printfn ""
    printfn @"            Wiktor Adamski prezentuje"
    printfn @".------..------..------..------..------..------."
    printfn @"|T.--. ||Y.--. ||S.--. ||I.--. ||Ą.--. ||C.--. |"
    printfn @"| :/\: || (\/) || :/\: || :(): || (\/) || :/\: |"
    printfn @"| (__) || :\/: || :\/: || ()() || :\/: || :\/: |"
    printfn @"| '--'T|| '--'Y|| '--'S|| '--'I|| '--'Ą|| '--'C|"
    printfn @"`------'`------'`------'`------'`------'`------'"
    printfn ""

    printf "Jak masz na imie? "
    let imie = Console.ReadLine().Trim()

    let rec dodajNaKoniec li el = 
        match li with
        | []    -> [el]
        | [x]   -> x :: [Array.map2 ( + ) x el]
        | x::xs -> x :: dodajNaKoniec xs el
    let gracze = [|
        {rodzaj = Czlowiek (); imie = imie; karty = []; wynik = 0}
        {rodzaj = CPU (); imie = "Janusz"; karty = []; wynik = 0}
        {rodzaj = CPU (); imie = "Marian"; karty = []; wynik = 0}
        |]
    let mutable historia = []
    let mutable graczRozpoczynajacy = 0
    while Array.forall (fun x -> x.wynik < 1000) gracze do
        let wyniki = runda gracze graczRozpoczynajacy
        historia <- dodajNaKoniec historia wyniki
        graczRozpoczynajacy <- (graczRozpoczynajacy + 1) % 3
        for i in 0 .. 1 do 
            gracze.[i].wynik <- gracze.[i].wynik + wyniki.[i]
            if gracze.[i].wynik < 1000 && gracze.[i].wynik > 900 then gracze.[i].wynik <- 900
        printfn ""
        printfn "--- Historia wyników ---"
        printfn ""
        HUD.Historia historia gracze
        printfn ""
    printfn ""
    printfn "Koniec Gry!"
    printfn ""
    Console.ForegroundColor <- kolorKonsoli
    0