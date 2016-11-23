(* Wiktor Adamski *)

(* Zadanie 1 *)
let zgadnij () =
  let porownaj liczba strzal =
    if liczba = strzal then "Zgadles. Brawo!"
    else if liczba < strzal then "moja jest mniejsza"
    else "moja jest wieksza"
  in
  let liczba = Random.int 101 and strzal = ref (-1) in
  while !strzal <> liczba do
    print_endline "Podaj liczbe:";
    strzal := read_int ();
    print_endline @@ porownaj liczba !strzal
  done;;
    
(* Zadanie 2 *)

let sortuj_plik () =
  let wczytaj_string plik =
    let bialy_znak zn = zn = ' ' || zn = '\n' || zn = '\t' in
    let s = ref "" and zn = ref (input_char plik) in
    begin
      while bialy_znak !zn do zn := input_char plik done;
      while  not( bialy_znak !zn) do
      s := !s ^ (String.make 1 !zn);
      zn := input_char plik;
    done;
    !s;
  end;
  in
  let wczytaj_int plik = int_of_string @@ wczytaj_string plik in
  let wczytaj_float plik = float_of_string @@ wczytaj_string plik in
  begin
    print_endline "Podaj nazwe pliku wejsciowego";
    let wej = open_in @@ read_line() in
    let rozmiar = wczytaj_int wej in
    let tablica = Array.init rozmiar (fun _ -> wczytaj_float wej) in
    close_in wej;
    Array.sort compare tablica;
    print_endline "Podaj nazwe pliku wyjsciowego";
    let wyj = open_out @@ read_line() in
    Array.iter (fun x -> output_string wyj @@ string_of_float x ^ " ") tablica;
    close_out wyj;
  end
  ;;
  