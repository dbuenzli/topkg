(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

module Ipc = Topkg_care_ipc
module Text = Topkg_care_text
module Change_log = Topkg_care_change_log
module Opam = Topkg_care_opam
module OCamlbuild = Topkg_care_ocamlbuild
module OCamlfind = Topkg_care_ocamlfind
module Browser = Topkg_care_browser
module Pager = Topkg_care_pager
module Editor = struct
  let edit_file f = match OS.Env.(value "EDITOR" (some cmd) ~absent:None) with
  | None -> Logs.err (fun m -> m "EDITOR environment variable undefined."); Ok 1
  | Some editor ->
      OS.Cmd.exists editor >>= function
      | true -> OS.Cmd.(run Cmd.(editor % p f)) >>= fun () -> Ok 0
      | false ->
          Logs.err (fun m -> m "Editor %a not in search path" Cmd.pp editor);
          Ok 1
end

module Std_files = Topkg_care_std_files
module Lint = Topkg_care_lint
module Distrib = Topkg_care_distrib
module Build = Topkg_care_build
module Delegate = Topkg_care_delegate
module Doc = struct

let pp_distrib ppf (name, version) =
  Fmt.pf ppf "%a %a"
    Fmt.(styled `Bold string) name
    Fmt.(styled `Cyan string) version

let log_publish_result msg distrib dir =
  Logs.app (fun m -> m "%s %a@ in@ directory@ %a@ of@ gh-pages@ branch"
               msg pp_distrib distrib Fpath.pp dir)

let publish_in_git_branch ~branch ~name ~version ~docdir ~dir =
  let cp src dst =
    let dst_is_root = Fpath.is_current_dir dst in
    let src =
      if dst_is_root then Fpath.to_dir_path src else Fpath.rem_empty_seg src
    in
    (* FIXME we lost Windows friends here, fix bos #30 *)
    OS.Cmd.run Cmd.(v "cp" % "-R" % p src % p dst)
  in
  let delete dir =
    if not (Fpath.is_current_dir dir) then OS.Dir.delete ~recurse:true dir else
    let delete acc p = acc >>= fun () -> OS.Path.delete ~recurse:true p in
    let gitdir = Fpath.v ".git" in
    let not_git p = not (Fpath.equal p gitdir) in
    OS.Dir.contents dir
    >>= fun files -> List.fold_left delete (Ok ()) (List.filter not_git files)
  in
  let git_for_repo r = Cmd.of_list (Topkg.Cmd.to_list @@ Topkg.Vcs.cmd r) in
  let replace_dir_and_push docdir dir =
    let msg = strf "Update %s doc to %s." name version in
    Topkg.Vcs.get ()
    >>= fun repo -> Ok (git_for_repo repo)
    >>= fun git -> OS.Cmd.run Cmd.(git % "checkout" % branch)
    >>= fun () -> delete dir
    >>= fun () -> cp docdir dir
    >>= fun () -> Topkg.Vcs.is_dirty repo
    >>= function
    | false -> Ok false
    | true ->
        OS.Cmd.run Cmd.(git % "add" % p dir)
        >>= fun () -> OS.Cmd.run Cmd.(git % "commit" % "-m" % msg)
        >>= fun () -> OS.Cmd.run Cmd.(git % "push")
        >>= fun () -> Ok true
  in
  if not (Fpath.is_rooted ~root:Fpath.(v ".") dir)
  then
    R.error_msgf
      "%a directory is not rooted in the repository or not relative"
      Fpath.pp dir
  else
  let clonedir = Fpath.(parent docdir / strf "%s-%s.pubdoc" name version) in
  OS.Dir.delete ~recurse:true clonedir
  >>= fun () -> Topkg.Vcs.get ()
  >>= fun repo -> Topkg.Vcs.clone repo ~dir:(Fpath.to_string clonedir)
  >>= fun () -> OS.Dir.with_current clonedir (replace_dir_and_push docdir) dir
  >>= function
  | false (* no changes *) ->
      log_publish_result "No documentation changes for" (name, version) dir;
      Ok ()
  | true ->
      let push_spec = strf "%s:%s" branch branch in
      Ok (git_for_repo repo) >>= fun git ->
      OS.Cmd.run Cmd.(git % "push" % "origin" % push_spec)
      >>= fun () -> OS.Dir.delete ~recurse:true clonedir
      >>= fun () ->
      log_publish_result "Published documentation for" (name, version) dir;
      Ok ()
end

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
