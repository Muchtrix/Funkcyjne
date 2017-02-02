namespace Tysiac

open System

module Common =

///////////////////////////////////////////////////////////////////////////////
// Typy                                                                      //
///////////////////////////////////////////////////////////////////////////////

    type Kolor = Pik | Trefl | Karo | Kier
    type Wartosc = Dziewiec | Walet | Dama | Krol | Dziesiec | As
    type Karta = Karta of Kolor * Wartosc
    type Stol = {mutable karty : Karta list;
             mutable atu : Kolor Option;
             mutable kol : Kolor Option;
             mutable pozostale: Karta list}

    [<AbstractClass>]
    type RodzajGracza () =
        abstract member Licytuj: Karta list -> Int32 -> Int32
        abstract member PodbijStawke: Karta list -> Int32 -> Int32
        abstract member ZagrajKarte: Karta list -> Stol -> Gracz [] -> Int32 list -> Karta
        abstract member OddajKarte: Karta list -> String -> Karta

    and Gracz = {rodzaj : RodzajGracza;
                     imie : String;
                     mutable karty : Karta list;
                     mutable wynik : Int32}

///////////////////////////////////////////////////////////////////////////////
// Podstawowe wartości i funkcje                                             //
///////////////////////////////////////////////////////////////////////////////
    let talia = 
        [ for kol in [Pik; Trefl; Karo; Kier] do
            for war in [Dziewiec; Walet; Dama; Krol; Dziesiec; As] -> Karta (kol, war)]

    let potasowana () =
        let ran = new Random() 
        Array.sortBy (fun _ -> ran.Next()) <| List.toArray talia

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
         t.[21 .. 23] |> Array.fold dodajDoReki [])

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
        let wyn karta:Int32 =
            let (Karta(k, w)) = karta
            if Some k = stol.atu then  13 + punktyZaKarte karta
                                 else if Some k = stol.kol then 1 + punktyZaKarte karta else 0
        fst << List.maxBy snd <| List.mapi (fun ind karta -> (ind, wyn karta)) stol.karty

    let wynikLewy (lewa: Karta list) =
        List.fold (fun aku k -> aku + punktyZaKarte k) 0 lewa