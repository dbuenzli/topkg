(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)
open Bos_setup

let () = Topkg.Pkg.prevent_standalone_main ()

(* Publish documentation *)

let repo_docdir_path_from_doc_uri uri =
  (* Parses the $PATH of http://$HOST/$PROJECT/$PATH *)
  let uri_error uri =
    R.error_msgf "Could not derive publication directory $PATH from OPAM doc \
                  field value %a; expected the pattern \
                  $SCHEME://$HOST/$PROJECT/$PATH" String.dump uri
  in
  match Topkg_care.Text.split_uri ~rel:true uri with
  | None -> uri_error uri
  | Some (_, _, path) ->
      if path = "" then uri_error uri else
      match String.cut ~sep:"/" path with
      | None | Some (_, "") -> Ok (Fpath.v ".")
      | Some (project, path) -> Fpath.of_string path >>| Fpath.rem_empty_seg

let publish_doc_gh_pages uri name version docdir =
  Fpath.of_string docdir
  >>= fun docdir -> repo_docdir_path_from_doc_uri uri
  >>= fun dir -> (Topkg_care.Doc.publish_in_git_branch
                    ~branch:"gh-pages" ~name ~version ~docdir ~dir)
  >>= fun () -> Ok 0

(* Publish releases *)

let create_release_json version msg =
  let escape_for_json s =
    let len = String.length s in
    let max = len - 1 in
    let rec escaped_len i l =
      if i > max then l else
      match String.get s i with
      | '\\' | '\"' | '\n' | '\r' | '\t' -> escaped_len (i + 1) (l + 2)
      | _  -> escaped_len (i + 1) (l + 1)
    in
    let escaped_len = escaped_len 0 0 in
    if escaped_len = len then s else
    let b = Bytes.create escaped_len in
    let rec loop i k =
      if i > max then Bytes.unsafe_to_string b else
      match String.get s i with
      | ('\\' | '\"' | '\n' | '\r' | '\t' as c) ->
          Bytes.set b k '\\';
          let c = match c with
          | '\\' -> '\\' | '\"' -> '\"' | '\n' -> 'n' | '\r' -> 'r'
          | '\t' -> 't'
          | _ -> assert false
          in
          Bytes.set b (k + 1) c; loop (i + 1) (k + 2)
      | c ->
          Bytes.set b k c; loop (i + 1) (k + 1)
    in
    loop 0 0
  in
  strf "{ \"tag_name\" : \"%s\", \
          \"body\" : \"%s\" }" (escape_for_json version) (escape_for_json msg)

let repo_and_owner_of_uri uri =
  let uri_error uri =
    R.error_msgf "Could not derive owner and repo from OPAM dev-repo \
                  field value %a; expected the pattern \
                  $SCHEME://$HOST/$OWNER/$REPO[.$EXT][/$DIR]" String.dump uri
  in
  match Topkg_care.Text.split_uri ~rel:true uri with
  | None -> uri_error uri
  | Some (_, _, path) ->
      if path = "" then uri_error uri else
      match String.cut ~sep:"/" path with
      | None -> uri_error uri
      | Some (owner, path) ->
          let repo = match String.cut ~sep:"/" path with
          | None -> path
          | Some (repo, path) -> repo
          in
          Fpath.of_string repo
          >>= fun repo -> Ok (owner, Fpath.(to_string @@ rem_ext repo))

let curl_create_release curl version msg owner repo =
  let parse_release_id resp = (* FIXME this is retired. *)
    try
      let headers = String.cuts ~sep:"\r\n" resp in
      let loc = List.find (String.is_prefix ~affix:"Location:") headers in
      let id = String.take ~rev:true ~sat:(fun c -> not (Char.equal '/' c)) loc
      in
      match String.to_int id with
      | None -> R.error_msgf "Could not parse id from location header %S" loc
      | Some id -> Ok id
    with Not_found ->
      R.error_msgf "Could not find release id in response %S." resp
  in
  let data = create_release_json version msg in
  let uri = strf "https://api.github.com/repos/%s/%s/releases" owner repo in
  let user = owner (* FIXME user and owner should be distinguished *) in
  let cmd = Cmd.(curl % "-u" % user % "-D" % "-" % "--data" % data % uri) in
  OS.Cmd.(run_out cmd |> to_string ~trim:false) >>= parse_release_id

let curl_upload_archive curl archive owner repo release_id =
  let uri =
      (* FIXME upload URI prefix should be taken from release creation
         response *)
      strf "https://uploads.github.com/repos/%s/%s/releases/%d/assets?name=%s"
        owner repo release_id (Fpath.filename archive)
    in
    let user = owner (* FIXME user and owner should be distinguished *) in
    let data = Cmd.(v "--data-binary" % strf "@%s" (Fpath.to_string archive)) in
    let ctype = Cmd.(v "-H" % "Content-Type:application/x-tar") in
    let cmd = Cmd.(curl % "-u" % user %% ctype %% data % uri) in
    OS.Cmd.(run cmd)

let publish_distrib uri name version msg archive =
  let git_for_repo r = Cmd.of_list (Topkg.Cmd.to_list @@ Topkg.Vcs.cmd r) in
  Fpath.of_string archive
  >>= fun archive -> OS.Cmd.must_exist Cmd.(v "curl")
  >>= fun curl -> Topkg.Vcs.get ()
  >>= fun repo -> Ok (git_for_repo repo)
  >>= fun git -> OS.Cmd.run Cmd.(git % "push" % "--force" % "--tags")
  >>= fun () -> repo_and_owner_of_uri uri
  >>= fun (owner, repo) -> curl_create_release curl version msg owner repo
  >>= fun id -> curl_upload_archive curl archive owner repo id
  >>= fun () -> Ok 0

(* Publish delegations *)

let unsupported = Ok 1

let publish = function
| "distrib" :: uri :: name :: version :: msg :: archive :: _ ->
    publish_distrib uri name version msg archive
| "doc" :: uri :: name :: version :: msg :: docdir :: _ ->
    publish_doc_gh_pages uri name version docdir
| "alt" :: kind :: uri :: name :: version :: msg :: archive :: _ ->
    unsupported
| args ->
    unsupported

(* Issue delegations *)

let issue = function
| "list" :: uri :: _ -> unsupported
| "show" :: uri :: id :: _ -> unsupported
| "open" :: uri :: title :: descr :: _ -> unsupported
| "close" :: uri :: id :: msg :: _ -> unsupported
| args -> unsupported

(* Delegation requests and main *)

let request = function
| "publish" :: args -> publish args
| "issue" :: args -> issue args
| args -> unsupported

let main () =
  let doc = "the GitHub toy delegate" in
  begin match OS.Arg.(parse ~doc ~pos:string ()) with
  | "ipc" :: verbosity :: req ->
      Logs.level_of_string verbosity
      >>= fun level -> Logs.set_level level; request req
  | "ipc" :: [] ->
      R.error_msg "malformed delegate request, verbosity is missing"
  | args ->
      R.error_msgf "unknown arguments: %s" (String.concat ~sep:" " args)
  end
  |> Logs.on_error_msg ~use:(fun () -> 2)

let () = exit (main ())
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
