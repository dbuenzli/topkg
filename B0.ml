open B0_kit.V000
open Result.Syntax

(* Just to define the opam files and use .opam publish for now. *)

(* Packs *)

let base_metadata =
  B0_meta.empty
  |> B0_meta.(add authors) ["The topkg programmers"]
  |> B0_meta.(add maintainers) ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
  |> B0_meta.(add homepage) "https://erratique.ch/software/topkg"
  |> B0_meta.(add online_doc) "https://erratique.ch/software/topkg/doc"
  |> B0_meta.(add licenses) ["ISC"]
  |> B0_meta.(add repo) "git+https://erratique.ch/repos/topkg.git"
  |> B0_meta.(add issues) "https://github.com/dbuenzli/topkg/issues"
  |> B0_meta.(add description_tags)
     ["packaging"; "ocamlbuild"; "org:erratique"]
  |> B0_meta.tag B0_opam.tag
  |> B0_meta.add B0_opam.build
    {|[["ocaml" "pkg/pkg.ml" "build" "--pkg-name" name
                                      "--dev-pkg" "%{dev}%"]]|}
let topkg =
  let meta =
    base_metadata
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.05.0"|};
        "ocamlfind", {|build & >= "1.6.1"|};
        "ocamlbuild", ""; ]
  in
  B0_pack.make "topkg" ~doc:"topkg package" ~meta ~locked:true []

let topkg_care =
  let meta =
    base_metadata
    |> B0_meta.add B0_release.src_archive_name (B0_pack.basename topkg)
    |> B0_meta.add B0_opam.depends
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
  B0_pack.make "topkg-care" ~doc:"topkg-care package" ~meta ~locked:true []
