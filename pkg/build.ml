#!/usr/bin/env ocaml

#directory "src" (* Bootstrap from source *)
#use "topkg.ml"

let () =
  Pkg.describe "topkg" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/topkg";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
