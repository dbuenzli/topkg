open B0_kit.V000
open B00_std
open Result.Syntax

(* Just to define the opam files and use .opam.pubish for now. *)

(* Packs *)

let base_metadata =
  let open B0_meta in
  empty
  |> add authors ["The topkg programmers"]
  |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
  |> add homepage "https://erratique.ch/software/topkg"
  |> add online_doc "https://erratique.ch/software/topkg/doc"
  |> add licenses ["ISC"]
  |> add repo "git+https://erratique.ch/repos/topkg.git"
  |> add issues "https://github.com/dbuenzli/topkg/issues"
  |> add description_tags ["packaging"; "ocamlbuild"; "org:erratique"]
  |> tag B0_opam.tag
  |> add B0_opam.Meta.build
    {|[["ocaml" "pkg/pkg.ml" "build" "--pkg-name" name
                                      "--dev-pkg" "%{dev}%"]]|}
let topkg =
  let meta =
    let open B0_meta in
    base_metadata
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.05.0"|};
        "ocamlfind", {|build & >= "1.6.1"|};
        "ocamlbuild", ""; ]
  in
  B0_pack.v "topkg" ~doc:"topkg package" ~meta ~locked:true []

let topkg_care =
  let meta =
    let open B0_meta in
    base_metadata
    |> add B0_release.Meta.src_archive_name (B0_pack.basename topkg)
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.05.0"|};
        "ocamlfind", {|build & >= "1.6.1"|};
        "ocamlbuild", "";
        "topkg", {|= version|};
        "fmt", "";
        "logs", "";
        "bos", {|>= "0.1.5"|};
        "cmdliner", {|>= "1.0.0"|};
        "webbrowser", "";
        "opam-format", {|>= "2.0.0"|};
      ]
  in
  B0_pack.v "topkg-care" ~doc:"topkg-care package" ~meta ~locked:true []
