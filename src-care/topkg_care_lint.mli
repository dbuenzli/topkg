(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Package linting

    See {!Topkg_care.Lint}. *)

open Astring
open Rresult

(** {1:lints Lints} *)

val custom : skip:bool -> Topkg.Pkg.lint -> int
(** [custom skip l] displays the outcome of [l]'s custom linting
    process (if any) and returns the number of errors. The test
    is not performed and the function returns 0 if [skip] is [true]
    or if no custom lint is specified. See also {!val:Topkg.Pkg.lint}. *)

val std_files : skip:bool -> Topkg.Pkg.std_files -> Topkg.Pkg.lint -> int
(** [std_files skip std l] performs standard file linting of all the
    files mentioned in [std] and in the files lint test description
    of [l] and returns the number of errors. The test is not performed
    and the function returns [0] if [skip] is [true] or if the test is
    disabled in [l]. See also {!val:Topkg.Pkg.lint}. *)

val meta : skip:bool -> Topkg.Pkg.std_files -> Topkg.Pkg.lint -> int
(** [files skip std l] performs ocamlfind META linting on all the
    meta files mentioned in [std]. The test is not performed and the
    function returns [0] if [skip] is [true] or if the test is
    disabled in [l]. See also {!val:Topkg.Pkg.lint}. *)

val opam : skip:bool -> Topkg.Pkg.std_files -> Topkg.Pkg.lint -> int
(** [files skip std l] performs OPAM file linting on all the OPAM
    files mentioned in [std]. The test is not performed and the
    function returns [0] if [skip] is [true] or if the test is
    disabled in [l]. See also {!val:Topkg.Pkg.lint}. *)

val deps : skip:bool -> Topkg.Pkg.std_files -> Topkg.Pkg.lint -> int
(** [deps skip std l] performs dependency linting between the
    OCamlbuild [_tags] file and all the OPAM files mentioned in [std].
    The test is not performed and the function returns [0] if [skip]
    is [true] or if the test is disabled in [l]. See also
    {!val:Topkg.Pkg.lint}. *)


(** {1:ldistrib Distribution linting} *)

val default_package_excludes : String.set
(** [default_package_excludes] is the union of
    {!OCamlfind.base_packages}, {!Opam.ocaml_base_packages},
    ["ocamlfind"], ["ocamlbuild"] and ["topkg"]. *)

type t = [ `Custom | `Std_files | `Meta | `Opam | `Deps ]
(** The type for lints. *)

val distrib :
  ?ignore_pkg:bool -> pkg_file:Fpath.t -> dir:Fpath.t -> skip:(t -> bool) ->
  (int, R.msg) result
(** [distrib ~ignore_pkg ~pkg_file ~dir ~skip] performs all the lints [l]
    with [skip l = false] on the distribution in [dir] using the package
    file [pkg_file] (relative to [pkg_file]) except if [ignore_pkg] is
    [true] (defaults to [false]). *)

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
