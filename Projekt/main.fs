open System

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

let talia = 
    [| for kol in [Pik; Trefl; Karo; Kier] do
       for war in [Dziewiec; Walet; Dama; Krol; Dziesiec; As] -> Karta (kol, war)|]

let potasowana () = 
    let t = talia.[0 .. 23] in
    let ran = Random() in 
    for x in 23 ..  -1 .. 1 do 
        let y = ran.Next(x) in
        let a = t.[x] in 
        t.[x] <- t.[y]
        t.[y] <- a
    t

let rec dodajDoReki reka (karta:Karta) =
    match reka with
    | []    -> [karta]
    | k::ks -> if karta < k then karta :: reka else k :: dodajDoReki ks karta

let rozdaj () = 
    let t = potasowana() in
    (t.[0  ..  6] |> Array.fold dodajDoReki [],
     t.[7  .. 13] |> Array.fold dodajDoReki [],
     t.[14 .. 20] |> Array.fold dodajDoReki [],
     t.[21 .. 23] |> Array.toList)

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
    | Dziesiec -> "1"
    | As       -> "A"

let pokazStol (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 []) =
    let dl       = (List.length stol.karty) - 1
    let poleDl   = String.length (Array.reduce (fun g1 g2 -> if String.length g1.imie < String.length g2.imie then g2 else g1) gracze).imie + 1
    let boki     = List.fold (fun aku _ -> aku + sprintf "%-*s" poleDl "+---+") "" stol.karty 
    let kolory   = List.fold (fun aku (Karta(k,_)) -> aku + (sprintf "%-*s" poleDl <| sprintf "| %s |" (symbolKolor k))) "" stol.karty
    let wartosci = List.fold (fun aku (Karta(_,w)) -> aku + (sprintf "%-*s" poleDl <| sprintf "| %s |" ( symbolWartosc w))) "" stol.karty
    let imiona   = List.fold (fun aku ind -> aku + (sprintf "%-*s" poleDl gracze.[kolejnosc.[ind]].imie)) "" [0 .. dl]
    [boki; kolory; wartosci; boki; imiona]

let wypiszStol (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 []) =
    for linijka in pokazStol stol gracze kolejnosc do
        printfn "%s" linijka    

let pokazRekeIStol (reka: Karta list) (dozw: Boolean list) (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 []) = 
    let lista = List.zip reka dozw
    let ustawKolor kol = if kol then Console.ForegroundColor <- ConsoleColor.White 
                                else Console.ForegroundColor <- ConsoleColor.DarkGray
    let c = System.Console.ForegroundColor
    let naStole = pokazStol stol gracze kolejnosc
    let mozeKolor = function Some k -> sprintf "%A" k | _ -> "brak"
    printfn "Kolor: %s, Atu: %s" (mozeKolor stol.kol) (mozeKolor stol.atu)
    printfn "%-*s | Twoja reka" (String.length naStole.[0]) "Stol"
    printfn "%*s-+-%s" 4 (String.replicate (String.length naStole.[0]) "-") (String.replicate (5 * List.length lista) "-")
    printf "%-*s | " 4 naStole.[0]
    for (_, bialy) in lista do
        ustawKolor bialy
        printf "+---+"
    printfn ""
    ustawKolor true
    printf "%-*s | " 4 naStole.[1]
    for (Karta(kol, _), bialy) in lista do
        ustawKolor bialy
        printf "| %s |" <| symbolKolor kol
    printfn ""
    ustawKolor true
    printf "%-*s | " 4 naStole.[2]
    for (Karta(_, war), bialy) in lista do
        ustawKolor bialy
        printf "| %s |" <| symbolWartosc war
    printfn ""
    ustawKolor true
    printf "%-*s | " 4 naStole.[3]
    for (_, bialy) in lista do
        ustawKolor bialy
        printf "+---+"
    printfn ""
    ustawKolor true
    printf "%-*s | " 4 naStole.[4]
    let mutable licznik = 1
    Console.ForegroundColor <- ConsoleColor.White
    for (_, bialy) in lista do
        if bialy then printf "  %d  " licznik
                      licznik <- licznik + 1
                 else printf "     "
    printfn ""
    Console.ForegroundColor <- c
let pokazReke (reka: Karta list) = 
    for _ in reka do
        printf "+---+"
    printfn ""
    for Karta(kol, _) in reka do
        printf "| %s |" <| symbolKolor kol
    printfn ""
    for Karta(_, war) in reka do
        printf "| %s |" <| symbolWartosc war
    printfn ""
    for _ in reka do
        printf "+---+"
    printfn ""
    for x in 1 .. List.length reka do
        printf "  %d  " x
    printfn ""
let rec usunKarte = function
        | ([], _)      -> []
        | (x::xs, kar) -> if x = kar then xs
                                     else let res = usunKarte (xs, kar) in x::res

let rec zagrajKarte (reka: Karta list) (stol: Stol) (gracze: GraczStr []) (kolejnosc: Int32 []) =
    let wKolorze = if (stol.kol = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.kol) reka
    let wAtu     = if (stol.atu = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.atu) reka
    let dozwolone = if List.isEmpty wKolorze then if List.isEmpty wAtu then reka 
                                                                       else wAtu 
                                             else wKolorze
    pokazRekeIStol reka (List.map (function k -> List.contains k dozwolone) reka) stol gracze kolejnosc
    let wybor = 
        try Int32.Parse <| Console.ReadLine()
        with _ -> 0 in
    if wybor >= 1 && wybor <= List.length dozwolone 
        then (dozwolone.[wybor - 1], usunKarte (reka, dozwolone.[wybor - 1]))
        else zagrajKarte reka stol gracze kolejnosc

let zagrajKarteCPU (reka: Karta list) (stol : Stol) =
    let wKolorze = if (stol.kol = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.kol) reka
    let wAtu     = if (stol.atu = None) then reka else List.filter (fun (Karta (k,_)) -> Some k = stol.atu) reka
    let dozwolone = if List.isEmpty wKolorze then if List.isEmpty wAtu then reka 
                                                                       else wAtu 
                                             else wKolorze
    (dozwolone.[0], usunKarte(reka, dozwolone.[0]))

let znajdzAtu (kolor : Kolor) =
    List.exists (fun (Karta(k, w)) -> kolor = k && (w = Krol || w = Dama))

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

let zwyciescaLewy (stol: Stol) =
    let wyn karta =
        let (Karta(k, w)) = karta
        if Some k = stol.atu then  10 * (punktyZaKarte karta)
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

let runda () = 
    let (r1, r2, r3, musik) = rozdaj()
    let gracze = [|
        {rodzaj = Czlowiek; imie = "Wiktor"; karty = r1; wynik = 0}
        {rodzaj = Cpu; imie = "Marek"; karty = r2; wynik = 0}
        {rodzaj = Cpu; imie = "Mariola"; karty = r3; wynik = 0}
        |]
    let mutable atu = None
    let kolejnosc = [| 0; 1; 2|]
    for _ in 1 .. 7 do
        let stol = {karty = []; atu = atu; kol = None; pozostale = Array.toList talia}
        for i in kolejnosc do
            let g = gracze.[i]
            let (Karta(k, w) as zagrana, reszta) =
                match g.rodzaj with
                | Czlowiek -> zagrajKarte g.karty stol gracze kolejnosc
                | Cpu      -> zagrajKarteCPU g.karty stol
            stol.pozostale <- usunKarte (stol.pozostale, zagrana)
            g.karty <- reszta
            if List.isEmpty stol.karty then                          // Wybór atu (pierwszy gracz w lewie)
                stol.kol <- Some k
                if (w = Krol || w = Dama) && znajdzAtu k reszta then // Sptawdzenie meldunku
                    stol.atu <- Some k
                    atu <- Some k
                    let p = punktyZaMeldunek k
                    printfn "Meldunek! %dpkt" p
                    g.wynik <- g.wynik + p
            stol.karty <- stol.karty @ [zagrana]
        wypiszStol stol gracze kolejnosc
        let punkty = wynikLewy stol.karty
        let zwyc   = kolejnosc.[zwyciescaLewy stol]
        gracze.[zwyc].wynik <- gracze.[zwyc].wynik + punkty
        printfn "%s wygrywa %dpkt" gracze.[zwyc].imie punkty
        for a in 0 .. 2 do
            kolejnosc.[a] <- (zwyc + a) % 3
    for g in gracze do
        printfn "%s wygrywa %dpkt" g.imie g.wynik