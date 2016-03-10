(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Astring
open Rresult
open Bos

let pp_not_found ppf () =
  Fmt.pf ppf "@[<v>No package delegate found.@,\
                   Try `topkg help delegate` for more information.@]"

(* OPAM file lookups *)

let opam_field f fields = match String.Map.find f fields with
| None | Some [] -> None
| Some (v :: _) -> Some v

let opam_issues_uri fields = match opam_field "bug-reports" fields with
| None -> "" | Some uri -> uri

let opam_releases_uri fields = match opam_field "dev-repo" fields with
| None -> "" | Some uri -> uri

let opam_doc_uri fields = match opam_field "doc" fields with
| None -> "" | Some uri -> uri

let opam_discover_delegate fields = match opam_field "homepage" fields with
| None -> None
| Some uri ->
    match Topkg_care_text.split_uri uri with
    | None -> None
    | Some (_, host, _) ->
        match List.rev (String.cuts ~sep:"." host) with
        | fst :: snd :: _ -> (Some (strf "%s-topkg-delegate" snd))
        | _ -> None

type t =
  { cmd : Cmd.t;
    issues_uri : string;
    releases_uri : string;
    doc_uri : string; }

let create cmd opam_fields =
  let issues_uri = opam_issues_uri opam_fields in
  let releases_uri = opam_releases_uri opam_fields in
  let doc_uri = opam_doc_uri opam_fields in
  { cmd; issues_uri; releases_uri; doc_uri}

let find ~pkg_file ~opam ~del =
  let opam_file opam std_files = match opam with
  | Some f -> Ok f
  | None -> Topkg_care_std_files.find_opam_file std_files
  in
  match del, opam with
  | Some cmd, Some opam ->
      Topkg_care_opam.File.fields opam
      >>= fun fields -> Ok (Some (create (Cmd.v cmd) fields))
  | _ ->
      let cmd_and_std_files =
        Topkg_care_ipc.ask ~pkg_file Topkg.Private.Ipc.delegate
        >>= fun (cmd, std_files) -> match del with
        | Some _ as cmd -> Ok (cmd, std_files)
        | None -> Ok (cmd, std_files)
      in
      cmd_and_std_files
      >>= function
      | (Some cmd, std_files) ->
          opam_file opam std_files
          >>= fun opam -> Topkg_care_opam.File.fields opam
          >>= fun fields -> Ok (Some (create (Cmd.v cmd) fields))
      | (None, std_files) ->
          match OS.Env.(value "TOPKG_DELEGATE" (some cmd) ~absent:None) with
          | Some cmd ->
              opam_file opam std_files
              >>= fun opam -> Topkg_care_opam.File.fields opam
              >>= fun fields -> Ok (Some (create cmd fields))
          | None ->
              opam_file opam std_files
              >>= fun opam -> Topkg_care_opam.File.fields opam
              >>= fun fields -> match opam_discover_delegate fields with
              | None -> Ok None
              | Some exec ->
                  let cmd = Cmd.v exec in
                  OS.Cmd.exists cmd >>= function
                  | true -> Ok (Some (create cmd fields))
                  | false ->
                      if exec <> "github-topkg-delegate" then Ok None else
                      let toy = Cmd.v "toy-github-topkg-delegate" in
                      Ok (Some (create toy fields))

(* Running the delegate *)

let run_delegate del args =
  let del = del.cmd in
  let verbosity = Logs.level_to_string (Logs.level ()) in
  let cmd = Cmd.(del % "ipc" % verbosity %% args) in
  OS.Cmd.run_status cmd >>= function
  | `Exited 0 -> Ok 0
  | `Exited 1 ->
      Logs.err (fun m -> m "Action unsupported by delegate %a" Cmd.pp del); Ok 1
  | (`Exited n | `Signaled n) ->
      Logs.err (fun m -> m "Delegate %a errored with %d" Cmd.pp del n); Ok n

(* Publish request *)

let publish_distrib ~del ~name ~version ~msg ~archive =
  run_delegate del Cmd.(v "publish" % "distrib" % del.releases_uri %
                        name % version % msg % p archive)

let publish_doc ~del ~name ~version ~msg ~docdir =
  run_delegate del Cmd.(v "publish" % "doc" % del.doc_uri %
                        name % version % msg % p docdir)

let publish_alt ~del ~kind ~name ~version ~msg ~archive =
  run_delegate del Cmd.(v "publish" % "alt" % del.releases_uri % kind %
                        name % version % msg % p archive)

(* Issue requests *)

let issue_list ~del =
  run_delegate del Cmd.(v "issue" % "list" % del.issues_uri)

let issue_show ~del ~id =
  run_delegate del Cmd.(v "issue" % "show" % del.issues_uri % "id")

let issue_open ~del ~title ~body =
  run_delegate del Cmd.(v "issue" % "open" % del.issues_uri % title % body)
  >>= function
  | 0 -> Ok 0
  | n ->
      let pp_body ppf = function "" -> () | body -> Fmt.pf ppf "@,@,%s" body in
      Logs.app (fun m -> m "@[<v>Your open issue message was:@,---@,%s%a@]"
                   title pp_body body);
      Ok n

let issue_close ~del ~id ~msg =
  run_delegate del Cmd.(v "issue" % "close" % del.issues_uri % id % msg)
  >>= function
  | 0 -> Ok 0
  | n ->
      Logs.app (fun m -> m "@[<v>Your closing message was:@,---@,%s@]" msg);
      Ok n

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
