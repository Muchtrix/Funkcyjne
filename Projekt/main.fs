open System

type Kolor = Pik | Trefl | Karo | Kier
type Wartosc = Dziewiec | Walet | Dama | Krol | Dziesiec | As
type Karta = Karta of Kolor * Wartosc
type Gracz = Czlowiek | Cpu
type GraczStr = {rodzaj : Gracz; mutable karty : Karta list; mutable wynik : Int32}

type Stol = {mutable karty : Karta list; mutable atu : Kolor Option; mutable kol : Kolor Option}

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

let wypiszKarta  = function
    |(Karta(k,w)) -> printf "%A %A" w k

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

let wczytajKolor = function
    | "pi" -> Pik
    | "tr" -> Trefl
    | "ka" -> Karo
    | "ki" -> Kier
    | _    -> Pik

let wczytajWartosc = function
    | "9" -> Dziewiec
    | "w" -> Walet
    | "d" -> Dama
    | "k" -> Krol
    | "1" -> Dziesiec
    | "a" -> As
    | _   -> As

let wczytajKarte (napis:string) = 
    let a = napis.Trim ' '
    let b = a.Split ' ' 
    Karta (wczytajKolor b.[0], wczytajWartosc b.[1])

let wczytajReke (reka:string) = reka.Split ',' |> Array.map wczytajKarte |> Array.toList

let wczytajZestaw (zestaw:string) = let rece = zestaw.Split '|' |> Array.map wczytajReke in (rece.[0], rece.[1], rece.[2]) 

// ki 1,ka a,tr 9|ka w|ka a

let wypiszReke (reka: Karta list) = 
    let dl = List.length reka in
    for _ in 1 .. dl do
        printf "+---+"
    printfn ""
    for Karta(kol, _) in reka do
        printf "| %s |" <| symbolKolor kol
    printfn ""
    for Karta(_, war) in reka do
        printf "| %s |" <| symbolWartosc war
    printfn ""
    for _ in 1 .. dl do
        printf "+---+"
    printfn ""
    for x in 1 .. dl do
        printf "  %d  " x
    printfn ""

let wypiszStol (stol: Stol) (gracze: GraczStr []) =
    let dl = (List.length stol.karty) - 1
    for _ in 0 .. 1 .. dl do
        printf "+---+"
    printfn ""
    for Karta(kol, _) in stol.karty do
        printf "| %s |" <| symbolKolor kol
    printfn ""
    for Karta(_, war) in stol.karty do
        printf "| %s |" <| symbolWartosc war
    printfn ""
    for _ in 0 .. 1 .. dl do
        printf "+---+"
    printfn ""
    for x in 0 .. 1 .. dl do
        printf "  %s" <| if gracze.[x].rodzaj = Czlowiek then "Ty " else "Cpu" 
    printfn ""
    

let rec wybierzKarte (reka: Karta list) (stol: Stol) =
    let odfiltrowane = if (stol.kol = None) then reka 
                                            else List.filter (fun (Karta (k, _)) -> Some k = stol.kol) reka
    let dozwolone = if List.isEmpty odfiltrowane then reka
                                                 else odfiltrowane
    let rec usunKarte = function
        | ([], _)      -> failwith "Programmer's a moron! in zagrajKarte"
        | (x::xs, kar) -> if x = kar then (x, xs)
                                     else let (k, res) = usunKarte (xs, kar) in
                                          (k, x::res)
    printfn "Twoja ręka:"
    wypiszReke reka
    printfn "Dostępne karty:"
    wypiszReke dozwolone
    let wybor = 
        try Int32.Parse <| Console.ReadLine()
        with _ -> 0 in
    if wybor >= 1 && wybor <= List.length dozwolone 
        then usunKarte (reka, reka.[wybor - 1])
        else wybierzKarte reka stol

let zagrajKarteCPU (reka: Karta list) (stol : Stol) = 
    let odfiltrowane = if (stol.kol = None) then reka 
                                            else List.filter (fun (Karta (k, _)) -> Some k = stol.kol) reka
    let dozwolone = if List.isEmpty odfiltrowane then reka 
                                                 else odfiltrowane
    let rec usunKarte = function
        | ([], _)      -> failwith "Programmer's a moron! in zagrajKarteCPU"
        | (x::xs, kar) -> if x = kar then (x, xs)
                                     else let (k, res) = usunKarte (xs, kar) in
                                          (k, x::res)
    let ind = Random().Next(List.length dozwolone)
    usunKarte(reka, reka.[ind])

let znajdzAtu (kolor : Kolor) = List.exists (fun (Karta(k, w)) -> kolor = k && (w = Krol || w = Dama))

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
    let (a, b, c, musik) = rozdaj()
    let gracze = [|
        {rodzaj = Czlowiek; karty = a; wynik = 0}
        {rodzaj = Cpu; karty = b; wynik = 0}
        {rodzaj = Cpu; karty = c; wynik = 0}
        |]
    let atu = ref None
    for _ in 1 .. 7 do
        let stol = {karty = []; atu = !atu; kol = None}
        for g in gracze do
            let (Karta(k, w) as zagrana, reszta) =
                match g.rodzaj with
                | Czlowiek -> 
                    wypiszStol stol gracze
                    printfn "Kolor: %A, Atu: %A" stol.kol stol.atu
                    wybierzKarte g.karty stol
                | Cpu      -> zagrajKarteCPU g.karty stol
            g.karty <- reszta
            if (w = Krol || w = Dama) && znajdzAtu k reszta then // Sptawdzenie meldunku
                stol.atu <- Some k
                atu := Some k
                let p = punktyZaMeldunek k
                printfn "Meldunek! %dpkt" p
                g.wynik <- g.wynik + p
            if List.isEmpty stol.karty then                      // Wybór atu (pierwszy gracz w lewie)
                stol.kol <- Some k
            stol.karty <- stol.karty @ [zagrana]
        wypiszStol stol gracze
        let punkty = wynikLewy stol.karty
        let zwyc   = zwyciescaLewy stol
        gracze.[zwyc].wynik <- gracze.[zwyc].wynik + punkty
        printfn "%A wygrywa %dpkt" (gracze.[zwyc].rodzaj) <| punkty
        if zwyc = 1 then 
            let tmp = gracze.[1]
            gracze.[1] <- gracze.[2]
            gracze.[2] <- gracze.[0]
            gracze.[0] <- tmp
        if zwyc = 2 then 
            let tmp = gracze.[2]
            gracze.[1] <- gracze.[0]
            gracze.[2] <- gracze.[1]
            gracze.[0] <- tmp
        
