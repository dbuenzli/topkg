(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let cmd =
  Cmd.of_list @@ Topkg.Cmd.to_list @@ Topkg.Env.tool "ocamlbuild" `Build_os

let find_packages ~roots s =
  let find_next_package_id =
    let package = String.sub "package(" in
    let not_rpar c = not (Char.equal ')' c) in
    let not_dot c = not (Char.equal '.' c) in
    fun ~root s -> match String.Sub.find_sub ~sub:package s with
    | None -> None
    | Some s ->
        let rest = String.Sub.(extend (stop s)) in
        let id, rest = String.Sub.span ~sat:not_rpar rest in
        let id = if root then String.Sub.take ~sat:not_dot id else id in
        Some (String.Sub.to_string id, String.Sub.tail rest)
  in
  let rec loop acc s = match find_next_package_id ~root:roots s with
  | None -> acc
  | Some (id, rest) -> loop (String.Set.add id acc) rest
  in
  loop String.Set.empty (String.sub s)

let package_tags ?(roots = false) file =
  OS.File.read file >>= fun contents -> Ok (find_packages ~roots contents)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
