open System

///////////////////////////////////////////////////////////////////////////////
// Typy                                                                      //
///////////////////////////////////////////////////////////////////////////////

type Kolor = Pik | Trefl | Karo | Kier
type Wartosc = Dziewiec | Walet | Dama | Krol | Dziesiec | As
type Karta = Karta of Kolor * Wartosc
type Gracz = Czlowiek | Cpu
type GraczStr = {rodzaj : Gracz;
                 imie : String;
                 mutable karty : Karta list;
                 mutable wynik : Int32}

type Stol = {mutable karty : Karta list;
             mutable atu : Kolor Option;
             mutable kol : Kolor Option;
             mutable pozostale: Karta list}

///////////////////////////////////////////////////////////////////////////////
// Podstawowe wartości i funkcje                                             //
///////////////////////////////////////////////////////////////////////////////
let talia = 
    [ for kol in [Pik; Trefl; Karo; Kier] do
       for war in [Dziewiec; Walet; Dama; Krol; Dziesiec; As] -> Karta (kol, war)]

let potasowana () = 
    let t = List.toArray talia.[0 .. 23] in
    let ran = Random() in 
    for x in 23 ..  -1 .. 1 do 
        let y = ran.Next(x) in
        let a = t.[x] in 
        t.[x] <- t.[y]
        t.[y] <- a
    t

let rec dodajDoReki (reka: Karta list) (karta: Karta) =
    match reka with
    | []    -> [karta]
    | k::ks -> if karta < k then karta :: reka else k :: dodajDoReki ks karta

let rec usunZReki (reka: Karta list) (karta: Karta) =
    match reka with
    | []    -> []
    | x::xs -> if x = karta then xs else x::(usunZReki xs karta)

let rozdaj () = 
    let t = potasowana() in
    (t.[0  ..  6] |> Array.fold dodajDoReki [],
     t.[7  .. 13] |> Array.fold dodajDoReki [],
     t.[14 .. 20] |> Array.fold dodajDoReki [],
     t.[21 .. 23] |> Array.toList)

let znajdzMeldunek (kolor : Kolor) (reka: Karta list) =
    List.forall (fun wartosc -> List.contains (Karta(kolor, wartosc)) reka) [Dama; Krol]

let punktyZaKarte (Karta(_, war)) = 
    match war with
    | Dziewiec -> 0
    | Walet    -> 2
    | Dama     -> 3
    | Krol     -> 4
    | Dziesiec -> 10
    | As       -> 11

let punktyZaMeldunek = function
    | Pik   -> 40
    | Trefl -> 60
    | Karo  -> 80
    | Kier  -> 100

let zwyciezcaLewy (stol: Stol) =
    let wyn karta =
        let (Karta(k, w)) = karta
        if Some k = stol.atu then  10 * ((punktyZaKarte karta) + 1)
                             else if Some k = stol.kol then punktyZaKarte karta
                                                       else 0
    let w0 = wyn stol.karty.[0]
    let w1 = wyn stol.karty.[1]
    let w2 = wyn stol.karty.[2]
    if (w0 > w1 && w0 > w2) then 0
                            else if (w1 > w0 && w1 > w2) then 1
                                                         else 2

let wynikLewy (lewa: Karta list) =
    List.fold (fun aku k -> aku + punktyZaKarte k) 0 lewa

///////////////////////////////////////////////////////////////////////////////
// Wyświetlanie stanu gry                                                    //
///////////////////////////////////////////////////////////////////////////////

let symbolKolor = function
    | Pik   -> "\u2660"
    | Trefl -> "\u2663"
    | Karo  -> "\u2666"
    | Kier  -> "\u2665"

let symbolWartosc = function
    | Dziewiec -> "9"
    | Walet    -> "W"
    | Dama     -> "D"
    | Krol     -> "K"
    | Dziesiec -> "10"
    | As       -> "A"

let pokazStol (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) =
    let dl       = (List.length stol.karty) - 1
    let poleDl   = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
    let boki     = List.fold (fun aku _ -> aku + sprintf "%-*s" poleDl "+---+") "" stol.karty 
    let kolory   = List.fold (fun aku (Karta(k,_)) -> aku + (sprintf "%-*s" poleDl <| sprintf "| %s |" (symbolKolor k))) "" stol.karty
    let wartosci = List.fold (fun aku (Karta(_,w)) -> aku + (sprintf "%-*s" poleDl <| sprintf "| %-2s|" ( symbolWartosc w))) "" stol.karty
    let imiona   = List.fold (fun aku ind -> aku + (sprintf "%-*s" poleDl gracze.[kolejnosc.[ind]].imie)) "" [0 .. dl]
    [boki; kolory; wartosci; boki; imiona]

let wypiszStol (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) =
    for linijka in pokazStol stol gracze kolejnosc do
        printfn "%s" linijka    

let pokazRekeIStol (reka: Karta list) (dozw: Boolean list) (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) = 
    let lista = List.zip reka dozw
    let kolorAktywny = Console.ForegroundColor
    let kolorNieaktywny = if (kolorAktywny = ConsoleColor.Gray) then ConsoleColor.DarkRed else ConsoleColor.Gray
    let ustawKolor kol = if kol then Console.ForegroundColor <- kolorAktywny
                                else Console.ForegroundColor <- kolorNieaktywny
    let naStole = pokazStol stol gracze kolejnosc
    let mozeKolor = function Some k -> sprintf "%A" k | _ -> "brak"
    let mutable licznik = 1
    printfn "Kolor: %s, Atu: %s" (mozeKolor stol.kol) (mozeKolor stol.atu)
    printfn "%-*s | Twoja ręka" (String.length naStole.[0]) "Stol"
    printfn "%4s-+-%s" (String.replicate (String.length naStole.[0]) "-") (String.replicate (5 * List.length lista) "-")
    let linijki = [   
                    (0, (fun _ _ -> printf "+---+"))
                    (1, (fun (Karta(kol, _)) _ -> printf "| %s |" <| symbolKolor kol))
                    (2, (fun (Karta(_, war)) _ -> printf "| %-2s|" <| symbolWartosc war))
                    (3, (fun _ _ -> printf "+---+"))
                    (4, (fun _ akt -> (if akt then printf "  %d  " licznik else printf "     "); licznik <- licznik + 1)) ] 
    for (ind, lam) in linijki do
        printf "%-4s | " naStole.[ind]
        for (karta, aktywna) in lista do
            ustawKolor aktywna
            lam karta aktywna
        printfn ""
        ustawKolor true

let pokazReke (reka: Karta list) (liczby: Boolean) = 
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

let wypiszHistorie (historia: Int32 [] list) (gracze:GraczStr []) = 
    let poleDl   = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
    let poprzeczka = String.replicate (poleDl + 2) "-"
    printfn " %-*s | %-*s | %-*s" poleDl gracze.[0].imie poleDl gracze.[1].imie poleDl gracze.[2].imie
    printfn "%s+%s+%s" poprzeczka poprzeczka poprzeczka
    for wpis in historia do
        printfn " %*d | %*d | %*d" poleDl wpis.[0] poleDl wpis.[1] poleDl wpis.[2]

///////////////////////////////////////////////////////////////////////////////
// Odczyt decyzji gracza (kod imperatywny)                                   //
///////////////////////////////////////////////////////////////////////////////

let rec licytacjaCzlowiek (reka: Karta list) (wynik: Int32) =
    pokazReke reka true
    printf "Czy chcesz przebić do %d? [t/n]: " <| wynik + 10
    let wybor = Console.ReadLine()
    if wybor = "t" then wynik + 10
        else if wybor = "n" then 0
    else licytacjaCzlowiek reka wynik

let rec podbicieLicytacji (reka: Karta list) (wynik: Int32) =
    pokazReke reka false
    printf "Podaj liczbę punktów o które chcesz zagrać [wielokrotność 10, min %d]: " wynik
    let wybor =
        try Int32.Parse <| Console.ReadLine()
        with _ -> 0
    if wybor >= wynik && wybor % 10 = 0 then wybor
                                        else podbicieLicytacji reka wynik
let rec zagrajKarte (reka: Karta list) (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 list) =
    let wKolorze = if (stol.kol = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.kol) reka
    let wAtu     = if (stol.atu = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.atu) reka
    let dozwolone = if List.isEmpty wKolorze then if List.isEmpty wAtu then reka 
                                                                       else wAtu 
                                             else wKolorze
    let mozliwe = List.map (fun k -> List.contains k dozwolone) reka
    pokazRekeIStol reka mozliwe stol gracze kolejnosc
    let wybor = 
        try Int32.Parse <| Console.ReadLine()
        with _ -> 0 in
    if wybor >= 1 && wybor <= List.length mozliwe && mozliwe.[wybor-1]
        then reka.[wybor - 1]
        else zagrajKarte reka stol gracze kolejnosc

let rec wybierzKarte (reka: Karta list) = 
    pokazReke reka true
    let wybor = 
        try Int32.Parse <| Console.ReadLine()
        with _ -> 0 in
    if wybor >= 1 && wybor <= List.length reka 
        then reka.[wybor - 1]
        else wybierzKarte reka

///////////////////////////////////////////////////////////////////////////////
// Gracze CPU                                                                //
///////////////////////////////////////////////////////////////////////////////

let CPUpewnyKolor (reka: Karta list) = 
    let ileKart = List.length reka
    let ileOdGory = 
        let wartosci = [As; Dziesiec; Krol; Dama; Walet; Dziewiec]
        List.zip (List.sortByDescending (fun (Karta(_,w)) -> w)reka) (List.take ileKart wartosci)
            |> List.takeWhile (fun (Karta(_,w), war) -> w = war)
            |> List.length
    ileKart + ileOdGory >= 6

let CPUpewnePunkty (reka: Karta list) =
    let ileKart = List.length reka
    let wartosci = [As; Dziesiec; Krol; Dama; Walet; Dziewiec]
    List.zip (List.sortByDescending (fun (Karta(_,w)) -> w)reka) (List.take ileKart wartosci)
            |> List.takeWhile (fun (Karta(_,w), war) -> w = war)
            |> List.map  (punktyZaKarte << fst)
            |> List.sum

let CPUpodzielNaKolory (reka: Karta list) =
    [Pik; Trefl; Karo; Kier]
        |> List.map (fun k -> (List.filter (fun (Karta(ko, _)) -> ko = k) reka, k)) 

let CPUsprawdzMeldunki (reka: Karta list) = 
    CPUpodzielNaKolory reka
        |> List.filter (fun (karty, kolor)-> znajdzMeldunek kolor karty)
        |> List.map snd
        |> List.rev

let CPUlicytacja (reka: Karta list) (wynik: Int32) = 
    let przewidywana = (wynikLewy reka) + 70
    if przewidywana >= wynik + 10 then wynik + 10
                                  else 0

let CPUwygrywajaceKarty (reka: Karta list) (stol: Stol) =
    let pozostale = (Set.ofList stol.pozostale) - (Set.ofList reka)
    let kombinacje k = 
        match stol.karty with
        | [k1; k2] -> [[k; k1; k2]]
        | [k1]     -> [for k2 in pozostale do if k1 <> k2 then yield [k; k1; k2]]
        | _        -> [for k1 in pozostale do for k2 in pozostale do if k1 <> k2 then yield [k; k1; k2]]
    let wygrywa k = List.forall id <| List.map (fun komb -> 0 = zwyciezcaLewy {stol with karty = komb; pozostale = []} ) (kombinacje k) 
                        
    let wyg = List.filter wygrywa reka |> List.sortBy (fun (Karta(_,w)) -> w)
    let prz = List.filter (not << wygrywa) reka |> List.sortBy (fun (Karta(_,w)) -> w)
    (wyg,prz)

let CPUwybierzKarte (reka: Karta list) =
    (List.sortBy (fun (Karta(_, w)) -> w) reka).[0]

let CPUzagrajKarte (reka: Karta list) (stol : Stol) =
    let wKolorze = if (stol.kol = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.kol) reka
    let wAtu     = if (stol.atu = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.atu) reka
    let dozwolone = if List.isEmpty wKolorze then if List.isEmpty wAtu then reka 
                                                                       else wAtu 
                                             else wKolorze
    let meldunki = CPUsprawdzMeldunki dozwolone
    let mozeMeldowac = List.isEmpty stol.karty && not <| List.isEmpty meldunki
    match CPUwygrywajaceKarty dozwolone stol with
    | (x::_, _)                 -> x
    | ([], _) when mozeMeldowac -> Karta(meldunki.[0], Dama)
    | ([], x::_)                -> x
    | _                         -> failwith "impossible"


///////////////////////////////////////////////////////////////////////////////
// Przebieg rozrywki (mnóstwo kodu imperatywnego)                            //
///////////////////////////////////////////////////////////////////////////////

let licytacja (gracze: GraczStr []) (musik: Karta list) (rozpoczynajacy: Int32) =
    let mutable akt = rozpoczynajacy
    let mutable zwyc = 0
    let mutable maks = 100
    let lic = [|1; 1; 1|]
    let mutable pozostali = 3
    while pozostali > 1 do
        akt <- (akt + 1) % 3
        while lic.[akt] = 0 do
            akt <- (akt + 1) % 3
        if gracze.[akt].rodzaj = Czlowiek then lic.[akt] <- licytacjaCzlowiek gracze.[akt].karty maks
                                          else lic.[akt] <- CPUlicytacja gracze.[akt].karty maks
        if lic.[akt] = 0 then printfn "%s pasuje" gracze.[akt].imie
                              pozostali <- pozostali - 1
                         else printfn "%s podbija do %d" gracze.[akt].imie lic.[akt]
                              zwyc <- akt
                              maks <- lic.[akt]
    printfn "%s wygrywa licytację z wynikiem %d" gracze.[zwyc].imie maks
    printfn "Musik:"
    pokazReke musik false
    gracze.[zwyc].karty <- List.fold dodajDoReki gracze.[zwyc].karty musik
    for i in 1 .. 2 do
        let k = if gracze.[zwyc].rodzaj = Czlowiek 
                then printfn "Wybierz kartę dla %s" gracze.[(zwyc + i) % 3].imie
                     wybierzKarte gracze.[zwyc].karty 
                else CPUwybierzKarte gracze.[zwyc].karty
        gracze.[zwyc].karty <- usunZReki gracze.[zwyc].karty k
        gracze.[(zwyc + i) % 3].karty <- dodajDoReki gracze.[(zwyc + i) % 3].karty k
    if gracze.[zwyc].rodzaj = Czlowiek then maks <- podbicieLicytacji gracze.[zwyc].karty maks
    (zwyc, maks)

let rozgrywka (pocz: Int32) (gracze: GraczStr []) = 
    let stol = {karty = []; atu = None; kol = None; pozostale = talia}
    let mutable kolejnosc = [pocz; (pocz + 1) % 3; (pocz + 2) % 3]
    for _ in 1 .. 8 do
        stol.karty <- []
        stol.kol <- None
        for i in kolejnosc do
            let g = gracze.[i]
            let Karta(k, w) as zagrana =
                match g.rodzaj with
                | Czlowiek -> zagrajKarte g.karty stol gracze kolejnosc
                | Cpu      -> CPUzagrajKarte g.karty stol
            if List.isEmpty stol.karty then                          // Wybór atu (pierwszy gracz w lewie)
                stol.kol <- Some k
                if (w = Krol || w = Dama) && znajdzMeldunek k g.karty then // Sprawdzenie meldunku
                    stol.atu <- Some k
                    let p = punktyZaMeldunek k
                    printfn "Meldunek! %dpkt" p
                    g.wynik <- g.wynik + p
            stol.pozostale <- usunZReki stol.pozostale zagrana
            g.karty        <- usunZReki g.karty zagrana
            stol.karty     <- stol.karty @ [zagrana]
        wypiszStol stol gracze kolejnosc
        let punkty = wynikLewy stol.karty
        let zwyc   = kolejnosc.[zwyciezcaLewy stol]
        gracze.[zwyc].wynik <- gracze.[zwyc].wynik + punkty
        printfn "Lewę wartą %d pkt. zdobywa %s" punkty gracze.[zwyc].imie
        kolejnosc <- [zwyc; (zwyc + 1) % 3; (zwyc + 2) % 3]
    for g in gracze do
        g.wynik <- ((g.wynik + 5)/10)*10
        printfn "%s wygrywa %d pkt" g.imie g.wynik
    [|gracze.[0].wynik; gracze.[1].wynik; gracze.[2].wynik|]

let runda (graczeInfo: GraczStr []) (rozpoczynajacy: Int32) =
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
    wyniki

[<EntryPoint>]
let gra args =
    printfn "         Wiktor Adamski prezentuje"
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
        {rodzaj = Czlowiek; imie = imie; karty = []; wynik = 0}
        {rodzaj = Cpu; imie = "Zdzisław"; karty = []; wynik = 0}
        {rodzaj = Cpu; imie = "Zdzisiek"; karty = []; wynik = 0}
        |]
    let mutable historia = []
    let mutable graczRozpoczynajacy = 0
    while (Array.maxBy (fun g -> g.wynik) gracze).wynik < 1000 do
        let wyniki = runda gracze graczRozpoczynajacy
        historia <- dodajNaKoniec historia wyniki
        graczRozpoczynajacy <- (graczRozpoczynajacy + 1) % 3
        for i in 0 .. 1 do 
            gracze.[i].wynik <- gracze.[i].wynik + wyniki.[i]
            if gracze.[i].wynik < 1000 && gracze.[i].wynik > 900 then gracze.[i].wynik <- 900
        printfn ""
        printfn "---  Wyniki  ---"
        printfn ""
        wypiszHistorie historia gracze
        printfn ""
    0