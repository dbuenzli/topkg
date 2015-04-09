topkg — The trivial OCaml package builder
-------------------------------------------------------------------------------

topkg is a trivial package builder for distributing OCaml software. It
provides a mechanism to describe an [OPAM](http://opam.ocaml.org/)
`$PKG.install` file and make corresponding invocations in your build
system.

topkg brings the following advantages:

1. It frees you from implementing an install procedure in your build
   system: this task is delegated to OPAM or to the `opam-installer`
   tool.

2. It doesn't reclaim control over your build system. It will only
   invoke it *once* with a list of targets determined from a package
   description and a build environment explicitely specified on the
   command line.
   
3. It's a simple approach implemented by a short [OCaml script](pkg/topkg.ml).

topkg has been designed with `ocamlbuild` in mind but it should be
usable with any other build system as long as it is able to understand
the targets topkg
[generates](#map-from-descriptions-to-targets-and-build-artefacts).

topkg is a single BSD3 licensed OCaml [script builder](pkg/topkg.ml)
that you add to your repo.

## Basic setup 

Your repository and distribution should have the following files, you
can arrange that differently but it is better to have a single set 
of conventions across packages.

* `pkg/META`, a [Findlib](http://projects.camlcity.org/projects/findlib.html) 
  `META` file.
* `pkg/build.ml`, the package builder written with `topkg.ml`.
   See [Package description](#package-description).
* [`pkg/topkg.ml`](pkg/topkg.ml), support for writing `build.ml`.
* `opam`, your package metadata, its dependencies and the instructions
   to build it. Having it at the root of your repository allows OPAM
   to use the file for build instructions and dependencies if the
   package is pinned.

The build instructions of your package are simply an invocation of
`pkg/build.ml` with a specification of the build environment as a
boolean key map on the command line. The invocation is very explicit,
all arguments are mandatory (this will be invoked by machines and it
helps debugging builds).

Here is how you should write that invocation in the `build:` field of
the `opam` file:

```
build: [
  "ocaml" "pkg/build.ml" "native=%{ocaml-native}%"
                         "native-dynlink=%{ocaml-native-dynlink}%"
                         "optionaldep=%{optionaldep:installed}%"
                         ... ]
```

This will execute your build system with a set of targets and generate
in the root directory of your distribution an OPAM `$PKG.install`
file.

After OPAM executed the `build:` commands of a package `pkg`, if there
is a file `pkg.install` at the root of the distribution, it uses it to
install the package. It will also use it to uninstall the package, no
need to specify anything in the `remove:` field of the `opam`
file. Note that in each case no `ocamlfind` invocation is needed, just
install your `META` file in `lib`.

If you need to support another package system you can invoke
`pkg/build.ml` as above and then manage installation and
uninstallation at a given `$DESTDIR` using the generated
`$PKG.install` file and the `opam-installer` tool distributed with
OPAM.

## Package description

An OPAM `$PKG.install` file is a description of a standard UNIX
install. It has fields for each of the standard directories (`lib`,
`bin`, `man`, `etc`, etc.). Each of these fields lists the files to
install in the corresponding directory (or subdirectories). See the
`.install` file
[specification](http://opam.ocaml.org/doc/manual/dev-manual.html#sec25)
in the OPAM developer manual for more information.

In topkg, the package build description file `pkg/build.ml` is simply
a *manual* specification of every file you want to put in each field
of the OPAM `$PKG.install` file by calling install field functions
[`Pkg.{lib,bin,doc,...}`](pkg/topkg.ml#L47). Here is an example of a
`pkg/build.ml` description for a single module library that also
installs a command line tool called `jsontrip`:

```ocaml
#!/usr/bin/env ocaml 
#directory "pkg"
#use "topkg.ml"

let () =
  Pkg.describe "jsonm" ~builder:`OCamlbuild [
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
```

This says that we are declaring a package named `jsonm` which means
that the generated install file will be `jsonm.install`. It also says
that we are using `ocamlbuild` as a build tool and that we want all the
files specified with `Pkg.lib` to be installed the `lib` directory,
those with `Pkg.bin` in `bin`, those with `Pkg.doc` in `doc` etc.

Now there are two things that are unsatisfactory with the above
declaration.

1. The set of files to generate for a library is usually always the 
   same and it's painful to write them down explicitely. This
   is solved by [extension sets](#extension-sets).
2. For the binaries, we usually don't want to install both byte and
   native code. We want to install one tool without the `byte` or
   `native` suffix, and the native one if available. This is solved
   by [auto binaries](#auto-binaries).

Using these features the above declaration can be reduced to: 

```ocaml
#!/usr/bin/env ocaml 
#directory "pkg"
#use "topkg.ml"

let () = 
  Pkg.describe "jsonm" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/jsonm";
    Pkg.bin ~auto:true "test/jsontrip";
    Pkg.doc "README.md"; 
    Pkg.doc "CHANGES.md"; ]
```

Optional builds and installs are handled by declaring boolean keys
specified on the command line (see [Environment](#environment)) and
using the `cond` optional argument of install field functions (see
[Conditions](#conditions)). The following example compiles the
`vgr_pdf` library only if both Uutf and Otfm are present and
`vgr_htmlc` only if js_of_ocaml is present:

```ocaml
#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let uutf = Env.bool "uutf"
let otfm = Env.bool "otfm"
let jsoo = Env.bool "jsoo"
let vgr_pdf = uutf && otfm
let () = 
  Pkg.describe "vg" ~builder:`OCamlbuild [
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
```

## Extension sets 

The install field functions have an optional `exts` argument. If
present these extensions are appended to the path given to the
function. The module [`Exts`](pkg/topkg.ml#L23) defines a few
predefined extension sets. For example a module library implemented in
`src/mylib.ml` can be declared by:

```ocaml
Pkg.lib ~exts:Exts.library_module "src/mylib"
```
which is, effectively, a shortcut for:

```ocaml 
Pkg.lib "src/mylib.mli";
Pkg.lib "src/mylib.cmti";
Pkg.lib "src/mylib.cmi";
Pkg.lib "src/mylib.cmx";
Pkg.lib "src/mylib.cma";
Pkg.lib "src/mylib.a";
Pkg.lib "src/mylib.cmxa";
Pkg.lib "src/mylib.cmxs";
```

## Auto binaries

For generating an installing native binaries if native code
compilation is available and byte code binaries if not you can use the
`auto` optional argument of `Pkg.bin` and `Pkg.sbin`. Using it with
`true` you can simply specify the binary name prefix. It will use the
base name as the name of the tool and ask for either a `.native` or
`.byte` target depending if native compilation is available or not.

```ocaml
Pkg.bin ~auto:true "src/mybinary" 
```

## Conditions

Conditional installation is handled through the optional argument
`cond` of install field functions. If `cond` is `false` it's neither
built nor installed. For example for a library that depends on the
presence of another:

```ocaml
let otherlib = Env.bool "otherlib" 
...
Pkg.lib ~cond:otherlib ~exts:Exts.library_module "src/mylib"
```

Conditions related to native code and native code dynamic linking 
availability happen automatically:

* In `Pkg.lib` paths ending with `.cmxs` are dropped if 
  `Env.native_dynlink` is `false` and paths ending with 
  `.a`, `.cmx`, `.cmxa` and `.cmxs` are dropped if 
  `Env.native` is `false`. 
  
* In `Pkg.{bin,sbin}` path ending with `.native` are dropped
  if `Env.native` is false.

## Environment

New boolean keys are added to the environment by calling `Env.bool
key`. To output a sample environment you can invoke the build script with
`--help`:

    ocaml pkg/build.ml --help

## Renaming and installing in subdirectories

By default install field functions use the basename of the path given
to the function as the install name. If you need to rename the build
artefact or install to a subdirectory you can use the `dst`
optional argument of install field functions. For example for a
library that needs to be installed in a `subdir` subdirectory of
`lib` use:

```
Pkg.lib ~exts:Exts.library_module ~dst:"subdir/mylib" "src/mylib"
```

## Handling cmt and cmti files

Since the OCaml tools generate `.cmt` and `.cmti` files only as a side
effect they are treated specially: they are not built. For
`ocamlbuild` you should add this line to your `_tags` file:

    <**/*.{ml,mli}> : bin_annot
    
this will build them as a side effect of other build invocations. In
the `$PKG.install` file generated by topkg the `cmt` and `cmti` files
are prefixed by a ? so that if they are not built (pre OCaml 4.01 for
`ocamlbuild`) the install doesn't fail.

## Verifying the build invocation and .install generation

For verifying the build invocation and the generated `.install` file
you can invoke `build.ml` with `explain` as the first argument:

    ocaml pkg/build.ml explain native=true native-dynlink=true ...

This will not invoke the build system but output on stdout diagnostic
information aswell as the build invocation and the generated
`$PKG.install` file.

In contrast to the build invocation it is allowed to invoke `explain`
with an empty or partial environment. 

    ocaml pkg/build.ml explain

In that case it will use `true` for each unspecified environment
boolean key.

## Map from descriptions to targets and build artefacts

Given an invocation `Pkg.{lib,bin,...} "$PATH"`, the system generates
a target `$PATH` for your build system and expects to find the build
artefact in `$BUILD/$PATH` where `$BUILD` is the build directory of
the build tool (e.g. `_build` for `ocamlbuild`).

## Updating `topkg.ml`

If you are using git [`bin/topkg-update`](bin/topkg-update) is a
script that automates the process of updating `topkg.ml` to the latest
version. For installing the script:

```
opam pin add topkg http://erratique.ch/repos/topkg.git
```

Then simply go your repo and type:

```
topkg-update
```

This will make sure you have no staged changes and that `topkg.ml` is not
modified. If that is the case it will update the script and commit the
change on the current branch.
