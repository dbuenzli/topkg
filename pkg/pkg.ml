#!/usr/bin/env ocaml

(* Bootstrap from source, note #mod_use is 4.01 *)

#use "topfind"
#require "result"
#directory "src"
#mod_use "topkg_result.ml"
#mod_use "topkg_string.ml"
#mod_use "topkg_log.ml"
#mod_use "topkg_fpath.ml"
#mod_use "topkg_cmd.ml"
#mod_use "topkg_os.ml"
#mod_use "topkg_vcs.ml"
#mod_use "topkg_conf.ml"
#mod_use "topkg_fexts.ml"
#mod_use "topkg_codec.ml"
#mod_use "topkg_opam.ml"
#mod_use "topkg_install.ml"
#mod_use "topkg_std_files.ml"
#mod_use "topkg_lint.ml"
#mod_use "topkg_build.ml"
#mod_use "topkg_distrib.ml"
#mod_use "topkg_pkg.ml"
#mod_use "topkg_ipc.ml"
#mod_use "topkg.ml"

open Topkg

let std_files =
  Pkg.std_files ~opam:["topkg.opam", false; "topkg-care.opam", false ]
    ()

let () =
  let lint = Pkg.lint ~deps_excluding:(Some []) () in
  let pkg_name = Env.string ~absent:(fun () -> Ok "topkg") "pkg-name" in
  let care = pkg_name = "topkg-care" in
  Pkg.describe pkg_name ~std_files ~lint [
    Pkg.lib ~cond:(not care) "pkg/META";
    Pkg.lib ~cond:(not care) "topkg.opam" ~dst:"opam";
    Pkg.lib ~cond:(not care) ~exts:Exts.module_library "src/topkg";
    Pkg.lib ~cond:care "topkg-care.opam" ~dst:"opam";
    Pkg.lib ~cond:care ~exts:Exts.module_library "src-care/topkg_care";
    Pkg.bin ~cond:care ~auto:true "src-bin/topkg_bin" ~dst:"topkg";
    Pkg.bin ~cond:care ~auto:true
      "src-bin/toy_github_delegate" ~dst:"toy-github-topkg-delegate";
    Pkg.doc ~cond:care "test/unsupportive-delegate";
    Pkg.doc ~cond:care "test/echo-delegate";
 ]
