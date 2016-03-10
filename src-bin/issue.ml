(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Bos

let get_id = function
| Some id -> Ok id
| None -> R.error_msgf "No issue ID specified"

let get_issue_msg ~info = function
| Some "" -> Ok None
| Some msg -> Ok (Some (String.cuts ~sep:"\n" msg))
| None ->
    let is_msg s = not (String.is_prefix "#" s) in
    let rec rem_white_prefix = function
    | l :: ls when String.for_all Char.Ascii.is_white l -> rem_white_prefix ls
    | ls -> ls
    in
    OS.File.tmp "topkg-issue-msg-%s"
    >>= fun f -> OS.File.write f info
    >>= fun () -> Topkg_care.Editor.edit_file f
    >>= function
    | 0 ->
        OS.File.read f >>= fun m ->
        let msg = List.filter is_msg (String.cuts ~sep:"\n" m) in
        begin match rem_white_prefix msg with
        | [] -> Ok None
        | lines -> Ok (Some lines)
        end
    | n ->
        Logs.err (fun m -> m "Editor exited with non-zero error code.");
        Ok None

(* Actions *)

let issue_show ~del ~id =
  get_id id >>= fun id -> Topkg_care.Delegate.issue_show ~del ~id

let issue_open ~del msg =
  let open_info =
    "\n\
     # Please enter an issue description. The first non-blank line will be\n\
     # the issue title and the rest the issue description. Lines starting\n\
     # with '#' will be ignored. An empty description aborts the action."
  in
  get_issue_msg ~info:open_info msg >>= function
  | None ->
      Logs.app (fun m -> m "Open issue aborted due to empty issue message.");
      Ok 1;
  | Some lines ->
      let title, body = match lines with
      | title :: body -> title, String.(trim @@ concat ~sep:"\n" body)
      | [] -> assert false
      in
      Topkg_care.Delegate.issue_open ~del ~title ~body

let issue_close ~del ~id msg =
  let close_info =
    "\n\
     # Please enter a closing message. Lines starting with '#' will\n\
     # be ignored. An empty message aborts the action."
  in
  get_id id
  >>= fun id -> get_issue_msg ~info:close_info msg
  >>= function
  | None ->
      Logs.app
        (fun m -> m "Close issue %s aborted due to empty issue message." id);
      Ok 1
  | Some lines ->
      let msg = String.(trim @@ concat ~sep:"\n" lines) in
      Topkg_care.Delegate.issue_close ~del ~id ~msg

(* Command *)

let issue () pkg_file opam del action id msg =
  begin
    Topkg_care.Delegate.find ~pkg_file ~opam ~del
    >>= function
    | None ->
        Logs.err (fun m -> m "%a" Topkg_care.Delegate.pp_not_found ()); Ok 1
    | Some del ->
        let ret = match action with
        | `List -> Topkg_care.Delegate.issue_list ~del
        | `Show -> issue_show ~del ~id
        | `Open -> issue_open ~del msg
        | `Close -> issue_close ~del ~id msg
        in
        ret >>= fun ret -> Ok ret
  end
  |> Cli.handle_error

(* Command line interface *)

open Cmdliner

let action =
  let action = ["list",`List; "show",`Show; "open",`Open; "close",`Close;] in
  let doc = strf "The action to perform. $(docv) must be one of %s."
      (Arg.doc_alts_enum action)
  in
  let cmd = Arg.enum action in
  Arg.(value & pos 0 cmd `List & info [] ~doc ~docv:"ACTION")

let id =
  let doc = "An issue ID of the package's issue tracker." in
  Arg.(value & pos 1 (some string) None & info [] ~doc ~docv:"ID")

let msg =
  let doc = "For $(b,open) and $(b,close), $(docv) is the issue message.
             Prevents the interactive prompt for the message."
  in
  let docv = "MSG" in
  Arg.(value & opt (some string) None & info ["m"; "message"] ~doc ~docv)

let doc = "interact with the package's issue tracker"
let man =
  [ `S "SYNOPSIS";
    `P "$(b,$(mname)) $(b,$(tname)) [$(i,OPTION)]... [$(i,ACTION)]...";
    `S "DESCRIPTION";
    `P "The $(b,$(tname)) command interacts with the package's issue
        tracker via the package delegate. See topkg-delegate(7) for more
        details.";
    `P "To consult the issues in a WWW browser invoke
        $(b,topkg browse issues), no delegate is needed for this.";
    `S "ACTIONS";
    `I ("$(b,list) (default)",
        "List open issues.");
    `I ("$(b,show) $(i,ID)",
        "Show information about issue $(i,ID).");
    `I ("$(b,open)",
        "Open a new issue.");
    `I ("$(b,close) $(i,ID)",
        "Close issue $(i,ID).");
    `S "ARGUMENTS";
    `S "OPTIONS";
  ] @ Cli.common_opts_man @ [
    `S "ENVIRONMENT VARIABLES";
    `I ("$(i,EDITOR)", "The editor used to edit issue messages.");
    `I ("$(i,TOPKG_DELEGATE)", "The package delegate to use,
        see topkg-delegate(7).");
  ] @ Cli.see_also ~cmds:[]

let cmd =
  let info = Term.info "issue" ~sdocs:Cli.common_opts ~doc ~man in
  let t = Term.(pure issue $ Cli.setup $ Cli.pkg_file $ Cli.opam_file $
                Cli.delegate $ action $ id $ msg) in
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
