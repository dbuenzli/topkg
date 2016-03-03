(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% v%%VERSION%%
  ---------------------------------------------------------------------------*)

(** The trivial OCaml package builder.

    See the {{!basics}basics}. *)

(** {1 Package} *)

(** Build environment access *)
module Env : sig

  (** {1 Environment} *)

  val bool : string -> bool
  (** [bool key] declares [key] as being a boolean key in the environment.
      Specifing key=(true|false) on the command line becomes mandatory. *)

  val native : bool
  (** [native] is [bool "native"]. *)

  val native_dynlink : bool
  (** [native_dylink] is [bool "native-dynlink"] *)
end

(** Exts defines sets of file extensions. *)
module Exts : sig

  type ext = [`Ext of string | `Obj | `Lib | `Dll | `Exe]
  (** The type for extensions. *)

  val interface : ext list
  (** [interface] is [[".mli"; ".cmi"; ".cmti"]] *)

  val interface_opt : ext list
  (** [interface_opt] is [".cmx" :: interface] *)

  val c_library : ext list
  (** [c_library] is the extension for C libraries. This is determined
      from [ocamlc -config]. *)

  val c_dll_library : ext list
  (** [c_dll_library] is the extension for C dynamic libraries. This
      is determined from [ocamlc -config]. *)

  val library : ext list
  (** [library] is [[".cma"; ".cmxa"; ".cmxs"] @ c_library] *)

  val module_library : ext list
  (** [module_library] is [(interface_opt @ library)]. *)

  val exe : ext list
  (** [exe] is the extension for executables. This is determined from
      [ocamlc -config]. *)

  val exts : string list -> ext list
  (** [exts sl] is [sl] as a list of extensions. *)

  val ext : string -> ext list
  (** [ext s] is [s] as a list of extensions. *)
end

(** Package description. *)
module Pkg : sig

  (** {1 Package builder} *)

  type builder =
  [ `OCamlbuild of string list
  | `OCamlbuild_no_ocamlfind of string list
  | `Other of string * string ]
  (** The type for build tools.
      {ul
      {- [`OCamlbuild args], [ocamlbuild] is invoked with `args` and
         `-use-ocamlfind`.}
      {- [`OCamlbuild_no_ocamlfind args], [ocamlbuild] is invoked with
         [args]}
      {- [`Other (tool, bdir)], tool [tool] is invoked that generates
         its build artefacts in [bdir].}} *)

  (** {1 Install moves} *)

  type moves
  (** The type for install moves. *)

  type field =
    ?built:bool -> ?cond:bool ->
    ?exts:[`Ext of string | `Obj | `Lib | `Dll | `Exe] list -> ?dst:string ->
    string -> moves
  (** The type for field install functions. A call
      [field cond exts dst path] generates install moves as follows:
      {ul
      {- If [built] is [true] (defaults), [path] is looked up relative
         to the build directory rather than the root directory of the
         distribution.}
      {- If [cond] is [false] (defaults to [true]), no move is generated.}
      {- If [exts] is present, generates a move for each path in
         the list [List.map (fun e -> path ^ e) exts].}
      {- If [dst] is present this path is used as the move destination
         (allows to install in subdirectories). If absent [dst] is
         [Filename.basename path].}} *)

  val lib : field
  val bin : ?auto:bool -> field
  (** If [auto] is true (defaults to false) generates
      [path ^ ".native"] if {!Env.native} is [true] and
      [path ^ ".byte"] if {!Env.native} is [false]. If
      [auto] is true it also adds {!Ext.exe} to the destination. *)

  val sbin : ?auto:bool -> field (** See {!bin}. *)
  val libexec : ?auto:bool -> field (** See {!bin}. *)
  val toplevel : field
  val share : field
  val share_root : field
  val etc : field
  val doc : field
  val misc : field
  val stublibs : field
  val man : field
  val describe : string -> builder:builder -> moves list -> unit
  (** [describe name builder moves] describes a package named [name] with
      builder [builder] and install moves [moves]. *)
end

(** {1:basics Basics}

topkg brings the following advantages:

{ol
{- It frees you from implementing an install procedure in your build
   system: this task is delegated to OPAM or to the [opam-installer]
   tool}
{- It doesn't reclaim control over your build system. It will only
   invoke it *once* with a list of targets determined from a package
   description and a build environment explicitely specified on the
   command line.}}

topkg has been designed with [ocamlbuild] in mind but it should be
usable with any other build system as long as it is able to understand
the targets topkg {{!map}generates}.

topkg is a single ISC licensed OCaml library without dependencies
you depend on tha you add to your repo.

{1:setup Basic setup}

Your repository and distribution should have the following files, you
can arrange that differently but it is better to have a single set
of conventions across packages.

{ul
{- [pkg/META], a [Findlib](http://projects.camlcity.org/projects/findlib.html)
  [META] file.}
{- [pkg/build.ml], the package builder written with [topkg.ml].
   See [Package description](#package-description)}
{- [[pkg/topkg.ml]](pkg/topkg.ml), support for writing [build.ml].}
{- [opam], your package metadata, its dependencies and the instructions
   to build it. Having it at the root of your repository allows OPAM
   to use the file for build instructions and dependencies if the
   package is pinned.}}

The build instructions of your package are simply an invocation of
[pkg/build.ml] with a specification of the build environment as a
boolean key map on the command line. The invocation is very explicit,
all arguments are mandatory (this will be invoked by machines and it
helps debugging builds).

Here is how you should write that invocation in the [build:] field of
the [opam] file:

{v
build: [
  "ocaml" "pkg/build.ml" "native=%{ocaml-native}%"
                         "native-dynlink=%{ocaml-native-dynlink}%"
                         "optionaldep=%{optionaldep:installed}%"
                         ... ]
v}

This will execute your build system with a set of targets and generate
in the root directory of your distribution an OPAM [$PKG.install]
file.

After OPAM executed the [build:] commands of a package [pkg], if there
is a file [pkg.install] at the root of the distribution, it uses it to
install the package. It will also use it to uninstall the package, no
need to specify anything in the [remove:] field of the [opam]
file. Note that in each case no [ocamlfind] invocation is needed, just
install your [META] file in [lib].

If you need to support another package system you can invoke
[pkg/build.ml] as above and then manage installation and
uninstallation at a given [$DESTDIR] using the generated
[$PKG.install] file and the [opam-installer] tool distributed with
OPAM.

{1:descr Package description}

An OPAM [$PKG.install] file is a description of a standard UNIX
install. It has fields for each of the standard directories ([lib],
[bin], [man], [etc], etc.). Each of these fields lists the files to
install in the corresponding directory (or subdirectories). See the
[.install] file
[specification](http://opam.ocaml.org/doc/manual/dev-manual.html#sec25)
in the OPAM developer manual for more information.

In topkg, the package build description file [pkg/build.ml] is simply
a *manual* specification of every file you want to put in each field
of the OPAM [$PKG.install] file by calling install field functions
[[Pkg.{lib,bin,doc,...}]](pkg/topkg.ml#L47). Here is an example of a
[pkg/build.ml] description for a single module library that also
installs a command line tool called [jsontrip]:

{[
#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "jsonm" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib "src/jsonm.mli";
    Pkg.lib "src/jsonm.cmti";
    Pkg.lib "src/jsonm.cmi";
    Pkg.lib "src/jsonm.cma";
    Pkg.lib "src/jsonm.cmx";
    Pkg.lib "src/jsonm.a";
    Pkg.lib "src/jsonm.cmxa";
    Pkg.lib "src/jsonm.cmxs";
    Pkg.bin "test/jsontrip.byte";
    Pkg.bin "test/jsontrip.native";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
]}

This says that we are declaring a package named [jsonm] which means
that the generated install file will be [jsonm.install]. It also says
that we are using [ocamlbuild] as a build tool and that we want all the
files specified with [Pkg.lib] to be installed the [lib] directory,
those with [Pkg.bin] in [bin], those with [Pkg.doc] in [doc] etc.

Now there are two things that are unsatisfactory with the above
declaration.

{ol
{- The set of files to generate for a library is usually always the
   same and it's painful to write them down explicitely. This
   is solved by [extension sets](#extension-sets).}
{- For the binaries, we usually don't want to install both byte and
   native code. We want to install one tool without the [byte] or
   [native] suffix, and the native one if available. This is solved
   by [auto binaries](#auto-binaries)}}

Using these features the above declaration can be reduced to:
{[
let () =
  Pkg.describe "jsonm" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/jsonm";
    Pkg.bin ~auto:true "test/jsontrip";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
]}
Optional builds and installs are handled by declaring boolean keys
specified on the command line (see [Environment](#environment)) and
using the [cond] optional argument of install field functions (see
[Conditions](#conditions)). The following example compiles the
[vgr_pdf] library only if both Uutf and Otfm are present and
[vgr_htmlc] only if js_of_ocaml is present:

{[
let uutf = Env.bool "uutf"
let otfm = Env.bool "otfm"
let jsoo = Env.bool "jsoo"
let vgr_pdf = uutf && otfm
let () =
  Pkg.describe "vg" ~builder:(`OCamlbuild []) [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/vg";
    Pkg.lib ~exts:Exts.module_library "src/vgr_svg";
    Pkg.lib ~cond:vgr_pdf ~exts:Exts.module_library "src/vgr_pdf";
    Pkg.bin ~cond:vgr_pdf ~auto:true "test/vecho";
    Pkg.lib ~cond:jsoo ~exts:Exts.module_library "src/vgr_htmlc";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "test/min_htmlc.html";
    Pkg.doc "test/min_htmlc.ml";
    Pkg.doc "test/min_pdf.ml";
    Pkg.doc "test/min_svg.ml"; ]
]}

{1:exts Extension sets}

The install field functions have an optional [exts] argument. If
present these extensions are appended to the path given to the
function. The module [[Exts]](pkg/topkg.ml#L23) defines a few
predefined extension sets. For example a module library implemented in
[src/mylib.ml] can be declared by:
{[
Pkg.lib ~exts:Exts.library_module "src/mylib"
]}
which is, effectively, a shortcut for:
{[
Pkg.lib "src/mylib.mli";
Pkg.lib "src/mylib.cmti";
Pkg.lib "src/mylib.cmi";
Pkg.lib "src/mylib.cmx";
Pkg.lib "src/mylib.cma";
Pkg.lib "src/mylib.a";
Pkg.lib "src/mylib.cmxa";
Pkg.lib "src/mylib.cmxs";
]}

{1:autobin Auto binaries}

For generating an installing native binaries if native code
compilation is available and byte code binaries if not you can use the
[auto] optional argument of [Pkg.bin] and [Pkg.sbin]. Using it with
[true] you can simply specify the binary name prefix. It will use the
base name as the name of the tool and ask for either a [.native] or
[.byte] target depending if native compilation is available or not.

{[
Pkg.bin ~auto:true "src/mybinary"
]}

{1:conds Conditions}

Conditional installation is handled through the optional argument
[cond] of install field functions. If [cond] is [false] it's neither
built nor installed. For example for a library that depends on the
presence of another:

{[
let otherlib = Env.bool "otherlib"
...
Pkg.lib ~cond:otherlib ~exts:Exts.library_module "src/mylib"
]}

Conditions related to native code and native code dynamic linking
availability happen automatically:

* In [Pkg.lib] paths ending with [.cmxs] are dropped if
  [Env.native_dynlink] is [false] and paths ending with
  [.a], [.cmx], [.cmxa] and [.cmxs] are dropped if
  [Env.native] is [false].

* In [Pkg.{bin,sbin}] path ending with [.native] are dropped
  if [Env.native] is false.

{1:env Environment}

New boolean keys are added to the environment by calling [Env.bool
key]. To output a sample environment you can invoke the build script with
[--help]:

    ocaml pkg/build.ml --help

{1:rename Renaming and installing in subdirectories}

By default install field functions use the basename of the path given
to the function as the install name. If you need to rename the build
artefact or install to a subdirectory you can use the [dst]
optional argument of install field functions. For example for a
library that needs to be installed in a [subdir] subdirectory of
[lib] use:

{[
Pkg.lib ~exts:Exts.library_module ~dst:"subdir/mylib" "src/mylib"
]}

{1:cmt Handling cmt and cmti files}

Since the OCaml tools generate [.cmt] and [.cmti] files only as a side
effect they are treated specially: they are not built. For
[ocamlbuild] you should add this line to your [_tags] file:

    <**/*.{ml,mli}> : bin_annot

this will build them as a side effect of other build invocations. In
the [$PKG.install] file generated by topkg the [cmt] and [cmti] files
are prefixed by a ? so that if they are not built (pre OCaml 4.01 for
[ocamlbuild]) the install doesn't fail.

{1:verify Verifying the build invocation and .install generation}

For verifying the build invocation and the generated [.install] file
you can invoke [build.ml] with [explain] as the first argument:

    ocaml pkg/build.ml explain native=true native-dynlink=true ...

This will not invoke the build system but output on stdout diagnostic
information aswell as the build invocation and the generated
[$PKG.install] file.

In contrast to the build invocation it is allowed to invoke [explain]
with an empty or partial environment.

    ocaml pkg/build.ml explain

In that case it will use [true] for each unspecified environment
boolean key.

{1:map Map from descriptions to targets and build artefacts}

Given an invocation [Pkg.{lib,bin,...} "$PATH"], the system generates
a target [$PATH] for your build system and expects to find the build
artefact in [$BUILD/$PATH] where [$BUILD] is the build directory of
the build tool (e.g. [_build] for [ocamlbuild]).
*)

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
