(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Topkg_result

type context = [ `Pin | `Dev | `Distrib ]

type t =
  { prepare_on_pin : bool;
    dir : Topkg_fpath.t;
    pre : context -> unit result;
    cmd : context -> Topkg_conf.os -> build_dir:Topkg_fpath.t -> Topkg_cmd.t;
    post : context -> unit result; }

let nop = fun _ -> Ok ()
let cmd _ os ~build_dir =
  let ocamlbuild = Topkg_conf.tool "ocamlbuild" os in
  Topkg_cmd.(ocamlbuild % "-use-ocamlfind" % "-classic-display" %
             "-build-dir" % build_dir)

let v
    ?(prepare_on_pin = true) ?(dir = "_build") ?(pre = nop) ?(cmd = cmd)
    ?(post = nop) () =
  { prepare_on_pin; dir; pre; cmd; post }

let prepare_on_pin b = b.prepare_on_pin
let dir b = b.dir
let pre b = b.pre
let cmd b = b.cmd
let post b = b.post

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
