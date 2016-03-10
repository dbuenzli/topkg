(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

(* Actions *)

let show change_log last last_version no_pager =
  let text =
    OS.File.read change_log >>= fun text ->
    if not (last || last_version) then Ok text else
    let flavour = Topkg_care.Text.flavour_of_fpath change_log in
    match Topkg_care.Change_log.last_version ?flavour text with
    | None -> R.error_msgf "%a: Could not parse change log." Fpath.pp change_log
    | Some (version, (header, changes)) ->
        Ok (if last_version then version else strf "%s\n%s" header changes)
  in
  let pager = match no_pager || last_version with
  | true -> Ok None
  | false -> Topkg_care.Pager.find ()
  in
  text
  >>= fun text -> pager
  >>= function
  | None -> Logs.app (fun m -> m "%s" text); Ok 0
  | Some pager -> OS.Cmd.(in_string text |> run_in pager) >>= fun () -> Ok 0

let commit change_log =
  let change_log = Fpath.to_string change_log in
  Topkg.Vcs.get ()
  >>= fun r -> Topkg.Vcs.file_is_dirty r change_log
  >>= function
  | false ->
      Logs.app (fun m -> m "No changes to commit in %s" change_log);
      Ok 0
  | true ->
      Topkg.Vcs.commit_files r ~msg:"Update change log." [change_log]
      >>= fun () -> Ok 0

(* Command *)

let log () pkg_file action change_log last last_version no_pager =
  begin
    let change_log = match change_log with
    | Some change_log -> Ok change_log
    | None ->
        Topkg_care.Std_files.of_pkg_file ~pkg_file
        >>= fun std_files -> Topkg_care.Std_files.change_log std_files
    in
    change_log
    >>= fun change_log -> match action with
    | `Show -> show change_log last last_version no_pager
    | `Edit -> Topkg_care.Editor.edit_file change_log
    | `Commit -> commit change_log
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  let action = [ "show", `Show; "edit", `Edit; "commit", `Commit] in
  let doc = strf "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let cmd = Arg.enum action in
  Arg.(value & pos 0 cmd `Show & info [] ~doc ~docv:"ACTION")

let no_pager =
  let doc = "Do not pipe the output into a pager. This automatically
             happens if the TERM environment variable is 'dumb'."
  in
  Arg.(value & flag & info ["no-pager"] ~doc)

let last =
  let doc = "Show only the change log of the last version. Extracted as the
             first marked up section of the change log."
  in
  Arg.(value & flag & info ["l"; "last"] ~doc)

let last_version =
  let doc = "Show only the version string of the last version. Extracted as
             the first token of the title of the first marked up section of
             the change log. Implies $(b,--no-pager).";
  in
  Arg.(value & flag & info ["t"; "last-version"] ~doc)

let doc = "show and edit the package's change log"
let man =
  [ `S "DESCRIPTION";
    `P "The $(b,$(tname)) command shows, edits and commits
        the package's change log.";
    `S "CHANGE LOG FORMAT";
    `P "To be able to extract the version and changes of the last distribution,
        a well defined change log format is assumed. Not abiding to the
        format is not catastrophic but may hinder or derail some facilities
        provided by topkg.";
    `P "The format assumes that the change log is written either in Markdown
        (default or .md extension) or Asciidoc (.asciidoc or .adoc extension).
        A change log is a list of marked up sections. A section is
        a header of any level until the next header at the same level or
        the end of file. For example here are two Markdown sections:";
    `Pre "\
v2.0.0
------
### New features
etc.
### Breaking changes
etc.

v1.6.0 1995-09-12
-----------------
etc.";
    `P "The first marked up section in the file is taken as being the
        change log for the last distribution; use $(b,topkg log -l)
        to check that it is parsed correctly. This is used by topkg-publish(1)
        and topkg-opam(1) to enrich distribution publication.";
    `P "The first token of a section's header title is taken as being the
        version string of the distribution; use $(b,topkg log -t) to check
        that it is parsed correctly. It is used by topkg-tag(1) to tag the
        source repository.";
    `S "ACTIONS";
    `I ("$(b,show) (default)", "shows the package's change log.");
    `I ("$(b,edit)", "edit the package's change log.");
    `I ("$(b,commit)", "commit changes made to the package's change log to the
                        VCS.");
    `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
    `I ("$(i,EDITOR)", "The editor used to edit the change log.");
    `I ("$(i,PAGER)", "The pager used to consult the change log.");
    `I ("$(i,TERM)", "See option $(b,--no-pager).");
  ] @ Cli.see_also ~cmds:["topkg-publish"; "topkg-tag"; "topkg-opam"]

let cmd =
  let info = Term.info "log" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure log $ Cli.setup $ Cli.pkg_file $ action $ Cli.change_log $
                last $ last_version $ no_pager)
  in
  (t, info)

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
