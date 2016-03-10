(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Rresult
open Astring
open Bos

let pp_fpathb = Fmt.styled `Bold Fpath.pp
let pp_status ppf = function
| `Ok -> Fmt.(brackets @@ styled_unit `Green " OK ") ppf ()
| `Fail -> Fmt.(brackets @@ styled_unit `Red "FAIL") ppf ()

let header = "LINT"

let skip_lint kind =
  Logs.info (fun m -> m ~header "Skip %a" Fmt.text kind); 0

let disabled_lint kind =
  Logs.info (fun m -> m ~header "Package@ disabled@ %a." Fmt.text kind); 0

(* Custom *)

let custom ~skip lint =
  let kind = "custom linting" in
  match Topkg.Private.Lint.custom lint with
  | None -> disabled_lint kind
  | Some None (* assert (skip = true) *)-> skip_lint kind
  | Some (Some results) ->
      let log status (`Msg msg) =
        Logs.app (fun m -> m "%a @[%a@]" pp_status status Fmt.text msg)
      in
      let report errs = function
      | Ok msg -> log `Ok msg; errs
      | Error msg -> log `Fail msg; errs + 1
      in
      List.fold_left report 0 results

(* Files *)

let lint_file_exists errs file =
  begin
    Fpath.of_string file >>= fun file ->
    OS.File.exists file >>| fun exists ->
    let log status =
      Logs.app @@ fun m ->
      m "%a @[File %a@ is@ present.@]" pp_status status pp_fpathb file
    in
    match exists with
    | true -> log `Ok; errs
    | false -> log `Fail; (errs + 1)
  end
  |> Logs.on_error_msg ~use:(fun () -> errs + 1)

let std_files ~skip std_files lint =
  let kind = "standard files linting" in
  match Topkg.Private.Lint.files lint with
  | None -> disabled_lint kind
  | Some files ->
      if skip then skip_lint kind else
      let std = Topkg.Private.Std_files.files std_files in
      let files = List.(rev_append (rev std) files) in
      List.fold_left lint_file_exists 0 files

(* External linter run *)

let run_external_linter cmd =
  OS.Cmd.(run_out ~err:err_run_out cmd |> out_string)
  >>| fun (out, status) -> match snd status with
  | `Exited 0 -> `Ok
  | _ -> `Fail out

let lint_file file_kind linter_name ~cmd errs file =
  begin
    Fpath.of_string file
    >>= fun file -> run_external_linter Cmd.(cmd % p file)
    >>| fun status -> match status with
    | `Ok ->
        Logs.app (fun m -> m "%a @[lint@ %s %a.@]"
                     pp_status `Ok file_kind pp_fpathb file);
        errs
    | `Fail msgs ->
        Logs.app
          (fun m -> m "%a @[<v>@[lint@ %s %a:@]@,@[%s messages:@]@]\n%s\n"
              pp_status `Fail file_kind pp_fpathb file linter_name msgs);
        errs + 1
  end
  |> Logs.on_error_msg ~use:(fun () -> errs + 1)

(* ocamlfind META files *)

let meta ~skip std_files lint =
  let kind = "ocamlfind META files linting" in
  match Topkg.Private.Lint.meta lint with
  | false -> disabled_lint kind
  | true ->
      if skip then skip_lint kind else
      let metas = Topkg.Private.Std_files.meta std_files in
      let metas = List.(rev @@ rev_map fst metas) in
      let cmd = Cmd.(Topkg_care_ocamlfind.cmd % "lint") in
      let lint = lint_file "META file" "ocamlfind lint" ~cmd in
      List.fold_left lint 0 metas

(* OPAM files *)

let opam ~skip std_files lint =
  let kind = "OPAM files linting" in
  match Topkg.Private.Lint.opam lint with
  | false -> disabled_lint kind
  | true ->
      if skip then skip_lint kind else
      let files = Topkg.Private.Std_files.opam std_files in
      let files = List.(rev @@ rev_map fst files) in
      let cmd = Cmd.(Topkg_care_opam.cmd % "lint") in
      let lint = lint_file "OPAM file" "opam lint" ~cmd in
      List.fold_left lint 0 files

(* Dependencies *)

let pp_deps_mismatches ppf (opam_only, tags_only) =
  let pp_miss present absent ppf id =
    Fmt.pf ppf "@[%a: %a present but %a absent@]"
      Fmt.(styled `Underline string) id
      Fmt.(styled `Green string) present
      Fmt.(styled `Red string) absent
  in
  let sep =
    if String.Set.(is_empty opam_only || is_empty tags_only)
    then Fmt.nop else Fmt.cut
  in
  Fmt.pf ppf "@[<v>%a%a%a@]"
    (String.Set.pp (pp_miss "opam" "_tags")) opam_only
    sep ()
    (String.Set.pp (pp_miss "_tags" "opam")) tags_only

let default_package_excludes =
  let exclude = ["ocamlfind"; "ocamlbuild"; "topkg"] in
  Topkg_care_ocamlfind.base_packages
  |> String.Set.union Topkg_care_opam.ocaml_base_packages
  |> String.Set.union (String.Set.of_list exclude)

let lint_deps ?(exclude = []) ~opam ~tags =
  begin
    Topkg_care_ocamlbuild.package_tags ~roots:true tags
    >>= fun tags -> Topkg_care_opam.File.fields opam
    >>| fun fields ->
    let exclude = String.Set.of_list exclude in
    let exclude = String.Set.union exclude default_package_excludes in
    let keep id =
      not (String.Set.mem id exclude) &&
      not (String.is_prefix "conf-" id)
    in
    let opam_deps = Topkg_care_opam.File.deps ~opts:true fields in
    let opam = String.Set.filter keep opam_deps in
    let tags = String.Set.filter keep tags in
    let opam_only = String.Set.diff opam tags in
    let tags_only = String.Set.diff tags opam in
    if String.Set.is_empty opam_only && String.Set.is_empty tags_only
    then None
    else Some (opam_only, tags_only)
  end
  |> R.reword_error_msg ~replace:true
    (fun msg -> R.msgf "could not lint dependencies: %s" msg)

let lint_deps ~exclude ~tags errs opam =
  begin
    Fpath.of_string opam >>= fun opam ->
    lint_deps ~exclude ~opam ~tags >>| function
    | None ->
        Logs.app (fun m -> m "%a @[OPAM file %a@ and %a dependency check."
                     pp_status `Ok pp_fpathb opam pp_fpathb tags);
        errs
    | Some miss ->
        Logs.app
          (fun m -> m "%a @[<v>@[File %a and %a dependency check:@]@,%a@]"
              pp_status `Fail pp_fpathb opam pp_fpathb tags
              pp_deps_mismatches miss);
        errs + 1
  end
  |> Logs.on_error_msg ~use:(fun () -> errs + 1)

let deps ~skip std_files lint =
  let kind = "dependency linting" in
  match Topkg.Private.Lint.deps_excluding lint with
  | None -> disabled_lint kind
  | Some exclude ->
      if skip then skip_lint kind else
      let files = Topkg.Private.Std_files.opam std_files in
      let files = List.(rev @@ rev_map fst files) in
      let lint_deps = lint_deps ~exclude ~tags:(Fpath.v "_tags") in
      List.fold_left lint_deps 0 files

(* Distrib *)

type t = [ `Custom | `Std_files | `Meta | `Opam | `Deps ]

let all ~skip stdf lint =
  let e0 = custom ~skip:(skip `Custom) lint in
  let e1 = std_files ~skip:(skip `Std_files) stdf lint in
  let e2 = meta ~skip:(skip `Meta) stdf lint in
  let e3 = opam ~skip:(skip `Opam) stdf lint in
  let e4 = deps ~skip:(skip `Deps) stdf lint in
  e0 + e1 + e2 + e3 + e4

let distrib ?(ignore_pkg = false) ~pkg_file ~dir ~skip =
  let lint () =
    let data =
      if ignore_pkg then Ok (Topkg.Pkg.std_files (), Topkg.Pkg.lint ()) else
      let custom = not (skip `Custom) in
      Topkg_care_ipc.ask ~pkg_file (Topkg.Private.Ipc.lint ~custom)
    in
    data >>| fun (std_files, lint) ->
    match all ~skip std_files lint with
    | 0 ->
        Logs.app (fun m -> m "%a lint@ %a %a"
                     pp_status `Ok pp_fpathb pkg_file
                     (Fmt.styled_unit `Green "success") ());
        0
    | n ->
        Logs.app (fun m -> m "%a lint@ %a@ %a:@ %d@ errors."
                     pp_status `Fail pp_fpathb pkg_file
                     (Fmt.styled_unit `Red "failure") () n);
        1
  in
  OS.Dir.with_current dir lint ()

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
