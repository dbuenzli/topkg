(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Bos_setup

(* Publish documentation *)

let repo_docdir_path_from_doc_uri uri =
  (* Parses the $PATH of $SCHEME://$HOST/$REPO/$PATH *)
  let uri_error uri =
    R.error_msgf "Could not derive publication directory $PATH from OPAM doc \
                  field value %a; expected the pattern \
                  $SCHEME://$HOST/$REPO/$PATH" String.dump uri
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
  >>= fun dir -> (Topkg_care.Delegate.publish_in_git_branch
                    ~branch:"gh-pages" ~name ~version ~docdir ~dir)
  >>= fun () -> Ok 0

(* Publish releases *)

let repo_and_owner_of_uri uri =
  let uri_error uri =
    R.msgf "Could not derive owner and repo from OPAM dev-repo \
            field value %a; expected the pattern \
            $SCHEME://$HOST/$OWNER/$REPO[.$EXT][/$DIR]" String.dump uri
  in
  match Topkg_care.Text.split_uri ~rel:true uri with
  | None -> Error (uri_error uri)
  | Some (_, _, path) ->
      if path = "" then Error (uri_error uri) else
      match String.cut ~sep:"/" path with
      | None -> Error (uri_error uri)
      | Some (owner, path) ->
          let repo = match String.cut ~sep:"/" path with
          | None -> path
          | Some (repo, path) -> repo
          in
          begin
            Fpath.of_string repo
            >>= fun repo -> Ok (owner, Fpath.(to_string @@ rem_ext repo))
          end
          |> R.reword_error_msg (fun _ -> uri_error uri)

let github_auth ~owner =
  OS.Env.(value "TOPKG_GITHUB_AUTH" string ~absent:owner)

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

let run_with_auth auth curl =
    let auth = strf "-u %s" auth in
    OS.Cmd.(in_string auth |> run_io curl)

let curl_create_release curl version msg owner repo =
  let parse_release_id resp = (* FIXME this is retired. *)
    let headers = String.cuts ~sep:"\r\n" resp in
    try
      let not_slash c = not (Char.equal '/' c) in
      let loc = List.find (String.is_prefix ~affix:"Location:") headers in
      let id = String.take ~rev:true ~sat:not_slash loc in
      match String.to_int id with
      | None -> R.error_msgf "Could not parse id from location header %S" loc
      | Some id -> Ok id
    with Not_found ->
      R.error_msgf "Could not find release id in response:\n%s."
        (String.concat ~sep:"\n" headers)
  in
  let data = create_release_json version msg in
  let uri = strf "https://api.github.com/repos/%s/%s/releases" owner repo in
  let auth = github_auth ~owner in
  let cmd = Cmd.(curl % "-D" % "-" % "--data" % data % uri) in
  run_with_auth auth cmd |> OS.Cmd.to_string ~trim:false
  >>= parse_release_id

let curl_upload_archive curl archive owner repo release_id =
  let uri =
      (* FIXME upload URI prefix should be taken from release creation
         response *)
      strf "https://uploads.github.com/repos/%s/%s/releases/%d/assets?name=%s"
        owner repo release_id (Fpath.filename archive)
  in
  let auth = github_auth ~owner in
  let data = Cmd.(v "--data-binary" % strf "@@%s" (Fpath.to_string archive)) in
  let ctype = Cmd.(v "-H" % "Content-Type:application/x-tar") in
  let cmd = Cmd.(curl %% ctype %% data % uri) in
  OS.Cmd.(run_with_auth auth cmd |> to_stdout)

let publish_distrib uri name version msg archive =
  let git_for_repo r = Cmd.of_list (Topkg.Cmd.to_list @@ Topkg.Vcs.cmd r) in
  Fpath.of_string archive
  >>= fun archive -> OS.Cmd.must_exist Cmd.(v "curl" % "-s" % "-S" % "-K" % "-")
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

(* Delegation requests *)

let request = function
| "publish" :: args -> publish args
| "issue" :: args -> issue args
| args -> unsupported

(* Delegate tool commands *)

let ipc_cmd args =
  begin match args with
  | verbosity :: req ->
      Logs.level_of_string verbosity
      >>= fun level -> Logs.set_level level; request req
  | [] ->
      R.error_msg "malformed delegate request, verbosity is missing"
  end
  |> Logs.on_error_msg ~use:(fun () -> 2)

let main_cmd () = `Help (`Pager, None)

(* Cli interface *)

open Cmdliner

let ipc_cmd =
  let doc = "Delegate request IPCs" in
  let man =
    [ `S "DESCRIPTION";
      `P "The $(b,$(tname)) command implements the topkg delegate protocol.
          See topkg-delegate(7) and $(b,$(mname) --help) for more
          information." ]
  in
  let args =
    let doc = "IPC call arguments" in
    Arg.(value (pos_all string [] & info [] ~doc ~docv:"ARG"))
  in
  let info = Term.info "ipc" ~doc ~man in
  let t = Term.(const ipc_cmd $ args) in
  (t, info)

let main_cmd =
  let doc = "Topkg's toy GitHub delegate" in
  let man =
   [ `S "DESCRIPTION";
     `P "$(b,$(mname)) is a toy topkg delegate for GitHub. It will disappear
         once a decent GitHub delegate emerges. For more
         information about topkg delegates, see topkg-delegate(7).";
     `P "This delegate only supports the following delegations:";
     `I ("$(b,topkg publish doc)",
         "Commits and pushes the documentation to the gh-branch of the
          source repository. The publication directory PATH in the branch is
          determined by matching the OPAM 'doc' field against the
          pattern SCHEME://HOST/REPO/PATH.");
     `I ("$(b,topkg publish distrib)",
         "This requires curl(1). Creates a GitHub release with the
         version and publication message given to the delegate and
         uploads the distribution archive as a release artefact. By
         default the username used for authentication is the name of
         the GitHub owner of the repo (determined from the
         $(i,DISTRIB_URI) URI, itself determined from the 'dev-repo'
         field of the OPAM file, see topkg-delegate(7) and topkg's API
         documentation for more details); in this case your GitHub
         password will be prompted twice on the cli by curl (ugh). You
         can use another user and/or specify the password using the
         TOPKG_GITHUB_AUTH environment variable with a username:token
         value, see $(i,https://developer.github.com/v3/auth/); the
         token or password will be given to curl via stdin.  Also bear
         in mind that error reporting (e.g. if the release already
         exists) is made of raw JSON responses and thus very
         user-unfriendly.")]
  in
  let version = "%%VERSION%%" in
  let info = Term.info "toy-github-topkg-delegate" ~version ~doc ~man in
  let t = Term.(ret (const main_cmd $ const ())) in
  (t, info)

let main () =
  Topkg.Private.disable_main ();
  match Term.eval_choice main_cmd [ipc_cmd] with
  | `Error _ -> exit 3
  | `Ok ret -> exit ret
  | _ -> exit 0

let () = main ()

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
