(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bos_setup

let cmd =
  Cmd.of_list @@ Topkg.Cmd.to_list @@ Topkg.Conf.tool "ocamlbuild" `Host_os

let find_packages ~roots s =
  let package = String.sub "package(" in
  let not_rpar c = not (Char.equal ')' c) in
  let not_dot c = not (Char.equal '.' c) in
  let is_comma c = Char.equal ',' c in
  let is_sep c = Char.Ascii.is_white c || is_comma c in
  let rec loop acc s = match String.Sub.find_sub ~sub:package s with
  | None -> acc
  | Some s ->
      let rest = String.Sub.(extend (stop s)) in
      let ids, rest = String.Sub.span ~sat:not_rpar rest in
      let ids = String.Sub.fields ~empty:false ~is_sep ids in
      let add_id acc id =
        let id = if roots then String.Sub.take ~sat:not_dot id else id in
        String.Set.add (String.Sub.to_string id) acc
      in
      let acc = List.fold_left add_id acc ids in
      loop acc (String.Sub.tail rest)
  in
  loop String.Set.empty (String.sub s)

let package_tags ?(roots = false) file =
  OS.File.read file >>= fun contents -> Ok (find_packages ~roots contents)
