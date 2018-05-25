type code = int
type name = string
type map  = code * Uchar.t * name

type date =
  { y : int
  ; m : int
  ; d : int
  ; hours   : int
  ; minutes : int
  ; seconds : int
  ; tz  : [ `UT | `GMT | `EST | `EDT | `CST | `CDT | `MST | `MDT | `PST | `PDT | `Military_zone of char | `TZ of int ]
          (* <3 Mr. MIME *)
  ; cw  : [ `KW ] }

let tz_of_string = function
  | "UT" -> `UT
  | "GMT" -> `GMT
  | "EST" -> `EST
  | "EDT" -> `EDT
  | "CST" -> `CST
  | "CDT" -> `CDT
  | "MST" -> `MST
  | "MDT" -> `MDT
  | "PST" -> `PST
  | "PDT" -> `PDT
  | m when String.length m = 1 ->
    (match String.get m 0 with
     | '\065' .. '\073'
     | '\075' .. '\090'
     | '\097' .. '\105'
     | '\107' .. '\122' -> `Military_zone (String.get m 0)
     | _ -> invalid_arg "tz_of_string")
  | s ->
    if String.length s = 5
    then match String.get s 0, int_of_string (String.sub s 1 4) with
      | '+', n -> `TZ n
      | '-', n -> `TZ (- n)
      | chr, _ -> invalid_arg "tz_of_string"
      | exception Invalid_argument _ -> invalid_arg "tz_of_string"
    else invalid_arg "tz_of_string"

let cw_of_string = function
  | "KW" -> `KW (* Kalenderwoche *)
  | _ -> invalid_arg "cw_of_string"

type t =
  { name         : string
  ; date         : date
  ; uni_version  : int * int
  ; tbl_version  : int * int
  ; fmt          : [ `A ]
  ; authors      : (Emile.phrase * [ `Mail of Emile.mailbox | `Uri of Uri.t ]) list }

let pp_tz ppf = function
  | `UT -> Fmt.string ppf "UT"
  | `GMT -> Fmt.string ppf "GMT"
  | `EST -> Fmt.string ppf "EST"
  | `EDT -> Fmt.string ppf "EDT"
  | `CST -> Fmt.string ppf "CST"
  | `CDT -> Fmt.string ppf "CDT"
  | `MST -> Fmt.string ppf "MST"
  | `MDT -> Fmt.string ppf "MDT"
  | `PST -> Fmt.string ppf "PST"
  | `PDT -> Fmt.string ppf "PDT"
  | `Military_zone x -> Fmt.char ppf x
  | `TZ x -> Fmt.pf ppf "%04d" x

let pp_cw ppf `KW = Fmt.string ppf "[KW]"

let pp_date ppf date =
  Fmt.pf ppf "%04d-%02d-%02d %02d:%02d:%02d %a %a"
    date.y date.m date.d
    date.hours date.minutes date.seconds
    pp_tz date.tz pp_cw date.cw

let pp_version ppf (a, b) = Fmt.pf ppf "@[<0>%d.%d@]" a b

let pp_format ppf `A = Fmt.string ppf "Format A"

let pp_author ppf (name, value) = match value with
  | `Mail mail ->
    Fmt.pf ppf "%a <mailbox:%a>" Emile.pp_phrase name Emile.pp_mailbox mail
  | `Uri uri ->
    Fmt.pf ppf "%a <uri:%a>" Emile.pp_phrase name Uri.pp_hum uri

let pp ppf t =
  Fmt.pf ppf "{@[<hov>name = %s;@ \
              date = %a;@ \
              unicode-version = %a;@ \
              table-version = %a;@ \
              fmt = %a;@ \
              authors = %a;@]}"
    t.name
    pp_date t.date
    pp_version t.uni_version
    pp_version t.tbl_version
    pp_format t.fmt
    Fmt.(Dump.list (hvbox pp_author)) t.authors

module Date =
struct
  open Angstrom

  let tz = take_while ((<>) ' ')
  let cw = char '[' *> take_while ((<>) ']') <* char ']' >>| cw_of_string

  let digit = satisfy Astring.Char.Ascii.is_digit >>| fun chr -> Char.code chr - 48
  let digit2 =
    digit >>= fun a ->
    digit >>= fun b -> return (a * 10 + b)
  let digit4 =
    digit >>= fun a ->
    digit >>= fun b ->
    digit >>= fun c ->
    digit >>= fun d -> return (a * 1000 + b * 100 + c * 10 + d)

  let date =
    digit4 >>= fun y -> char '-' *>
    digit2 >>= fun m -> char '-' *>
    digit2 >>= fun d -> char ' ' *>
    digit2 >>= fun hours   -> char ':' *>
    digit2 >>= fun minutes -> char ':' *>
    digit2 >>= fun seconds -> char ' ' *>
    tz >>| tz_of_string >>= fun tz -> char ' ' *>
    cw >>= fun cw ->

    return { y; m; d; hours; minutes; seconds; tz; cw; }
end

module Binding =
struct
  open Angstrom

  let wsp = skip_while Astring.Char.Ascii.is_white

  let binding =
    take_while ((<>) ':') >>= fun name -> char ':' *> wsp *>
    take_while1 (fun _ -> true) >>= fun value ->
    return (Astring.String.trim name, Astring.String.trim value)
end

module Version =
struct
  open Angstrom

  let int = take_while1 Astring.Char.Ascii.is_digit >>| int_of_string
  let version = int >>= fun a -> char '.' *> int >>= fun b -> return (a, b)
end

module Format =
struct
  open Angstrom

  let format = string "Format A" >>| fun _ -> `A
end

module Author =
struct
  open Angstrom

  let author = Emile.Parser.phrase >>= fun name -> char '<' *> take_while ((<>) '>') <* char '>' >>| fun value ->
    match Emile.of_string value with
    | Ok mail -> name, `Mail mail
    | Error _ -> name, `Uri (Uri.of_string value)
end

module Option =
struct
  let map f v = match v with Some x -> (f x) | None -> None
  let ( >>= ) v f = map f v
end

let (<.>) f g = fun x -> f (g x)

let version = Rresult.R.to_option <.> Angstrom.parse_string Version.version
let date    = Rresult.R.to_option <.> Angstrom.parse_string Date.date
let format  = Rresult.R.to_option <.> Angstrom.parse_string Format.format
let author  = Rresult.R.to_option <.> Angstrom.parse_string Author.author

let descr source =
  let open Option in

  let header, _ = List.partition Source.is_comment source in
  let bindings =
    List.fold_left
      (fun a line -> match Angstrom.parse_string Binding.binding line with
         | Ok v -> v :: a
         | Error _ -> a) [] (List.map (function Source.Comment x -> x | _ -> assert false) header) in
  let bindings = List.rev bindings in

  let name = match List.assoc_opt "Name" bindings with
    | Some value -> value
    | None -> invalid_arg "Invalid ISO8859 file: no name" in
  let unicode_version = match List.assoc_opt "Unicode version" bindings >>= version with
    | Some value -> value
    | None -> invalid_arg "Invalid ISO8859 file: no unicode version" in
  let table_version = match List.assoc_opt "Table version" bindings >>= version with
    | Some value -> value
    | None -> invalid_arg "Invalid ISO8859 file: no unicode version" in
  let table_format = match List.assoc_opt "Table format" bindings >>= format with
    | Some value -> value
    | None -> invalid_arg "Invalid ISO8859 file: no format" in
  let date = match List.assoc_opt "Date" bindings >>= date with
    | Some value -> value
    | None -> invalid_arg "Invalid ISO8859 file: no valid date" in

  (* XXX(dinosaure): sometimes, we can have more than 1 author, FIXME! *)
  let authors = match List.assoc_opt "Authors" bindings >>= author with
    | Some value -> [ value ]
    | None -> [] in

  { name
  ; date
  ; uni_version = unicode_version
  ; tbl_version = table_version
  ; fmt         = table_format
  ; authors }

let descr source =
  try Ok (descr source)
  with Invalid_argument err -> Rresult.R.error_msg err

let maps source =
  let _, maps = List.partition Source.is_comment source in
  List.fold_left (fun map -> function
      | Source.Map { a; b; name; } -> Ptmap.add a (b, (Astring.String.trim name)) map
      | _ -> assert false) Ptmap.empty maps
  |> Rresult.R.ok
