open B0_kit.V000
open Result.Syntax

(* Note we have much more deps than in ocamlbuild here because we don't do
   transitive -I. *)

let opam_core = B0_ocaml.libname "opam-core"
let opam_format = B0_ocaml.libname "opam-format"
let cmdliner = B0_ocaml.libname "cmdliner"
let rresult = B0_ocaml.libname "rresult"
let astring = B0_ocaml.libname "astring"
let fpath = B0_ocaml.libname "fpath"
let fmt = B0_ocaml.libname "fmt"
let fmt_tty = B0_ocaml.libname "fmt.tty"
let fmt_cli = B0_ocaml.libname "fmt.cli"
let logs = B0_ocaml.libname "logs"
let logs_fmt = B0_ocaml.libname "logs.fmt"
let logs_cli = B0_ocaml.libname "logs.cli"
let bos_setup = B0_ocaml.libname "bos.setup"
let webbrowser = B0_ocaml.libname "webbrowser"
let webbrowser_cli = B0_ocaml.libname "webbrowser.cli"

let topkg = B0_ocaml.libname "topkg"
let topkg_care = B0_ocaml.libname "topkg.care"

(* Libraries *)

let topkg_lib =
  B0_ocaml.lib topkg ~name:"topkg-lib" ~srcs:[`Dir ~/"src"]

let care_requires requires =
  opam_core :: opam_format:: cmdliner:: rresult :: astring :: fpath :: fmt ::
  fmt_tty :: fmt_cli :: logs :: logs_fmt :: logs_cli:: bos_setup ::
  webbrowser :: webbrowser_cli :: topkg :: requires

let topkg_care_lib =
  let srcs = [`Dir ~/"src-care"] in
  B0_ocaml.lib topkg_care ~srcs ~requires:(care_requires [])

(* Tools *)

let topkg_tool =
  let srcs = [`Dir ~/"src-bin"; `X ~/"src-bin/toy_github_delegate.ml" ] in
  B0_ocaml.exe ~public:true "topkg" ~srcs ~requires:(care_requires [topkg_care])

let toy_github_delegate =
  let srcs = [`File ~/"src-bin/toy_github_delegate.ml"] in
  let requires = [cmdliner; fpath; rresult; fmt_tty; astring; bos_setup; topkg;
                  topkg_care]
  in
  B0_ocaml.exe ~public:true "toy-github-topkg-delegate" ~srcs ~requires


(* Packs *)

let base_metadata =
  B0_meta.empty
  |> ~~ B0_meta.authors ["The topkg programmers"]
  |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
  |> ~~ B0_meta.homepage "https://erratique.ch/software/topkg"
  |> ~~ B0_meta.online_doc "https://erratique.ch/software/topkg/doc"
  |> ~~ B0_meta.licenses ["ISC"]
  |> ~~ B0_meta.repo "git+https://erratique.ch/repos/topkg.git"
  |> ~~ B0_meta.issues "https://github.com/dbuenzli/topkg/issues"
  |> ~~ B0_meta.description_tags ["packaging"; "ocamlbuild"; "org:erratique"]
  |> B0_meta.tag B0_opam.tag
  |> ~~ B0_opam.build
    {|[["ocaml" "pkg/pkg.ml" "build" "--pkg-name" name
                                      "--dev-pkg" "%{dev}%"]]|}
let topkg =
  let meta =
    base_metadata
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build & >= "1.6.1"|};
        "ocamlbuild", ""; ]
  in
  B0_pack.make "topkg" ~doc:"topkg package" ~meta ~locked:true []

let topkg_care =
  let meta =
    base_metadata
    |> ~~ B0_release.src_archive_name (B0_pack.basename topkg)
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build & >= "1.6.1"|};
        "ocamlbuild", "";
        "topkg", {|= version|};
        "fmt", {|>= "0.9.0"|};
        "logs", "";
        "bos", {|>= "0.2.1"|};
        "cmdliner", {|>= "1.3.0"|};
        "webbrowser", "";
        "opam-format", {|>= "2.0.0"|};
      ]
  in
  B0_pack.make "topkg-care" ~doc:"topkg-care package" ~meta ~locked:true []
