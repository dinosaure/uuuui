module M1 = Iso8859_1
module M2 = Iso8859_2
module M3 = Iso8859_3
module M4 = Iso8859_4
module M5 = Iso8859_5
module M6 = Iso8859_6
module M7 = Iso8859_7
module M8 = Iso8859_8
module M9 = Iso8859_9
module M10 = Iso8859_10
module M11 = Iso8859_11
module M13 = Iso8859_13
module M14 = Iso8859_14
module M15 = Iso8859_15
module M16 = Iso8859_16

let padding on str =
  if String.length str < on
  then String.((make (on - length str) ' ') ^ str)
  else str

let padding_needed =
  let maxv _cp (_ucp, name) acc = max (String.length name) acc in
  List.fold_left max 0
    [ Ptmap.fold maxv M1.database 0
    ; Ptmap.fold maxv M2.database 0
    ; Ptmap.fold maxv M3.database 0
    ; Ptmap.fold maxv M4.database 0
    ; Ptmap.fold maxv M5.database 0
    ; Ptmap.fold maxv M6.database 0
    ; Ptmap.fold maxv M7.database 0
    ; Ptmap.fold maxv M8.database 0
    ; Ptmap.fold maxv M9.database 0
    ; Ptmap.fold maxv M10.database 0
    ; Ptmap.fold maxv M11.database 0
    ; Ptmap.fold maxv M13.database 0
    ; Ptmap.fold maxv M14.database 0
    ; Ptmap.fold maxv M15.database 0
    ; Ptmap.fold maxv M16.database 0 ]

let ornament = "====="

let pp_general_category ppf = function
  | `Cc -> Fmt.string ppf "cc"
  | `Cf -> Fmt.string ppf "cf"
  | `Cn -> Fmt.string ppf "cn"
  | `Co -> Fmt.string ppf "co"
  | `Cs -> Fmt.string ppf "cs"
  | `Ll -> Fmt.string ppf "ll"
  | `Lm -> Fmt.string ppf "lm"
  | `Lo -> Fmt.string ppf "lo"
  | `Lt -> Fmt.string ppf "lt"
  | `Lu -> Fmt.string ppf "lu"
  | `Mc -> Fmt.string ppf "mc"
  | `Me -> Fmt.string ppf "me"
  | `Mn -> Fmt.string ppf "mn"
  | `Nd -> Fmt.string ppf "nd"
  | `Nl -> Fmt.string ppf "nl"
  | `No -> Fmt.string ppf "no"
  | `Pc -> Fmt.string ppf "pc"
  | `Pd -> Fmt.string ppf "pd"
  | `Pe -> Fmt.string ppf "pe"
  | `Pf -> Fmt.string ppf "pf"
  | `Pi -> Fmt.string ppf "pi"
  | `Po -> Fmt.string ppf "po"
  | `Ps -> Fmt.string ppf "ps"
  | `Sc -> Fmt.string ppf "sc"
  | `Sk -> Fmt.string ppf "sk"
  | `Sm -> Fmt.string ppf "sm"
  | `So -> Fmt.string ppf "so"
  | `Zl -> Fmt.string ppf "zl"
  | `Zp -> Fmt.string ppf "zp"
  | `Zs -> Fmt.string ppf "zs"

let is_cp uucd_database cp (ucp, name) =
  let is_cp = Uucd.is_cp ucp in
  let pp_valid ppf = function
    | true -> (Fmt.styled `Green Fmt.string) ppf "[OK]"
    | false -> (Fmt.styled `Red Fmt.string) ppf "[ERROR]" in

  let prop = Uucd.cp_prop uucd_database ucp Uucd.general_category in

  Fmt.(pf stdout) "%s:%04x is a valid Unicode code point: %a (general category property: %a).\n%!"
    (padding padding_needed name) ucp pp_valid is_cp
    Fmt.(option pp_general_category) prop; is_cp

let set_color style_renderer =
  Fmt_tty.setup_std_outputs ?style_renderer ()

let main channel () =
  let ic = match channel with `File filename -> open_in filename | `Stdin -> stdin in
  let decoder = Uucd.decoder (`Channel ic) in

  match Uucd.decode decoder with
  | `Error e ->
    let (l0, c0), (l1, c1) = Uucd.decoded_range decoder in
    `Error (false, Fmt.strf "Invalid Unicode database (%d.%d-%d.%d): %s" l0 c0 l1 c1 e)
  | `Ok uucd_database ->
    let () = match channel with `File _ -> close_in ic | `Stdin -> () in

    let databases =
      [ 1, M1.database
      ; 2, M2.database
      ; 3, M3.database
      ; 4, M4.database
      ; 5, M5.database
      ; 6, M6.database
      ; 7, M7.database
      ; 8, M8.database
      ; 9, M9.database
      ; 10, M10.database
      ; 11, M11.database
      ; 13, M13.database
      ; 14, M14.database
      ; 15, M15.database
      ; 16, M16.database ] in

    let res =
      let check (id, database) =
        Fmt.(pf stdout) "%a Check ISO8859-%d %a\n\n%!"
          Fmt.(styled `Magenta string) ornament
          id
          Fmt.(styled `Magenta string) ornament;
        let res = Ptmap.for_all (is_cp uucd_database) database in
        Fmt.(pf stdout) "\n%!"; res in
      List.for_all check databases in

    if res then `Ok ()
    else `Error (false, "At least, one code point is not a valid Unicode code point.")

open Cmdliner

let input =
  let parser filename =
    if Sys.file_exists filename
    then Ok (`File filename)
    else Error (`Msg (Fmt.strf "Invalid file: %s" filename)) in
  let printer ppf = function
    | `File filename -> Fmt.string ppf filename
    | `Stdin -> Fmt.string ppf "<stdin>" in
  Arg.conv ~docv:"<FILE>" (parser, printer)

let set_color = Term.(const set_color $ Fmt_cli.style_renderer ())

let uucd_database =
  Arg.(value & opt input `Stdin & info ["database"] ~docv:"<FILE>")

let cmd =
  let doc = "Tests databases." in
  let exits = Term.default_exits in
  Term.(ret (const main $ uucd_database $ set_color)),
  Term.info "iso8859" ~version:"<none>" ~doc ~exits

let () = Term.(exit @@ eval cmd)
