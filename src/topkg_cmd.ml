(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Command line fragments *)

type t = string list

let empty = []
let is_empty = function [] -> true | _ -> false
let v a = [a]
let ( % ) l a = a :: l
let ( %% ) l0 l1 = List.rev_append (List.rev l1) l0
let add_arg l a = l % a
let add_args l a = l %% a
let on bool l = if bool then l else []
let p f = f

(* Predicates and comparison *)

let equal l l' = l = l'
let compare l l' = compare l l'

(* Conversions and pretty printing *)

let to_rev_list line = line
let to_list line = List.rev line
let of_list ?slip line = match slip with
| None -> List.rev line
| Some slip -> List.fold_left (fun acc v -> v :: slip :: acc) [] line

let dump ppf cmd =
  let pp_elt ppf s = Format.fprintf ppf "%s" (Filename.quote s) in
  let rec loop = function
  | [] -> ()
  | v :: vs ->
      if vs = [] then pp_elt ppf v else
      (Format.fprintf ppf "%a@ " pp_elt v; loop vs)
  in
  Format.fprintf ppf "@[<1>["; loop (List.rev cmd); Format.fprintf ppf "]@]"
