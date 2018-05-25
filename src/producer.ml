open Ast_mapper
open Ast_helper
open Parsetree

let int n = Exp.constant (Const.int n)
let string s = Exp.constant (Const.string s)

let ptmap ~loc ptmap =
  Ptmap.fold
    (fun codepoint (unicode, comment) acc ->
      [%expr Ptmap.add
          [%e (int codepoint)]
          ([%e (int unicode)], [%e (string comment)])
          [%e acc]])
    ptmap [%expr Ptmap.empty]

let map ~loc ~add ~empty map =
  Ptmap.fold
    (fun codepoint (unicode, comment) acc ->
      [%expr [%e add]
          [%e (int codepoint)]
          ([%e (int unicode)], [%e (string comment)])
          [%e acc]])
    map [%expr [%e empty]]
