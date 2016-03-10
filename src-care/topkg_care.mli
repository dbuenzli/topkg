(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Topkg package care.

    Tools to help the package developer in the life cycle of the
    package. Most of these tools can be invoked directly from the
    command line via the [topkg] binary installed by the [topkg-care]
    package.

    {b WARNING.} Do not use this API in your package description file, use
    only {!Topkg}. This API was not thoroughly designed, is not stable and
    may change even between minor versions of [topkg]. Use at your own
    risk.

    {e %%VERSION%% - {{:%%PKG_WWW%% }homepage}} *)

open Astring
open Rresult
open Bos

(** {1 Helpers} *)

(** Text processing helpers.

    {b Warning.} These functions are not serious and can break on certain
    valid inputs in all sorts of fashion. To understand breakage bear
    in mind that they operate line-wise. *)
module Text : sig

  (** {1 Marked-up text files} *)

  type flavour = [ `Markdown | `Asciidoc ]
  (** The type for text document formats. *)

  val flavour_of_fpath : Fpath.t -> flavour option
  (** [flavour_of_fpath p] determines a flavour according to the
      extension of [p] as follows:
      {ul
      {- [Some `Markdown] for [.md]}
      {- [Some `Asciidoc] for [.asciidoc] or [.adoc]}
      {- [None] otherwise}} *)

  val head : ?flavour:flavour -> string -> (string * string) option
  (** [head ~flavour text] extracts the {e head} of the document [text] of
      flavour [flavour] (defaults to [`Markdown]).

      The head is defined as follows:
      {ul
      {- Anything before the first header is discarded.}
      {- The first header is kept in the first component}
      {- Everything that follows until the next header of the same or greater
         level is kept discarding trailing blank lines.}} *)

  val header_title : ?flavour:flavour -> string -> string
  (** [header_title ~flavour text] extract the title of a header [text]
      of flavour [flavour] (defaults to [`Markdown]. *)

  (** {1 Toy URI parsing} *)

  val split_uri : ?rel:bool -> string -> (string * string * string) option
  (** [split_uri uri] splits [uri] into a triple [(scheme, host, path)]. If
      [rel] is [true] (defaults to [false]), a leading ["/"] in [path] is
      removed. *)
end

(** Change log helpers. *)
module Change_log : sig

  (** {1 Change log} *)

  val last_version :
    ?flavour:Text.flavour -> string -> (string * (string * string)) option
    (** [last_version ~flavour text] is [Some (version, (header, text))]
        if a version number and extracts the version number and
        change log of the last version in [text]. *)
end

(** [OPAM] helpers. *)
module Opam : sig

  (** {1:cmd Command} *)

  val cmd : Cmd.t
  (** [cmd] is a command for [opam] looked up using
      {!Topkg.Env.OCaml.tool}[ "opam" `Build_os]. *)

  (** {1:pkgs Packages} *)

  val ocaml_base_packages : String.set
  (** [ocaml_base_packages] are the base OPAM packages distributed
      with OCaml: ["base-bigarray"], ["base-bytes"], ["base-threads"],
      ["base-unix"]. *)

  (** {1:file Files} *)

  (** OPAM files *)
  module File : sig

    (** {1:file OPAM file} *)

    val field_names : String.set
    (** [field_names] is the maximal domain of the map returned by
        {!fields}, excluding extension fields (not yet supported by
        [opam-lib] 1.2.2). *)

    val fields : Fpath.t -> ((string list) String.map , R.msg) result
    (** [fields f] returns a simplified model of the fields of the OPAM
        file [f]. The domain of the result is included in
        {!field_names}. Note that the [depends:] and [depopts:] fields
        are returned without version contsraints. *)

    (** {1:deps Dependencies} *)

    val deps : ?opts:bool -> (string list) String.map -> String.set
    (** [deps ~opts fields] returns the packages mentioned in the [depends:]
        fields, if [opts] is [true] (default) those from [depopts:] are added
        aswell. *)
  end

  (** {1 Descr} *)

  (** [descr] file. *)
  module Descr : sig

    (** {1:descr Descr file} *)

    type t = string * string
    (** The type for OPAM [descr] files, the package synopsis and the
        description. *)

    val of_readme_md : string -> (t, R.msg) result
    (** [of_readme_md f] extracts an OPAM description file from a README.md
        with a certain structure. *)

    val of_string : string -> (t, R.msg) result
    (** [of_string s] is a description from the string [s]. *)

    val to_string : t -> string
    (** [to_string d] is [d] as a string. *)
  end
end

(** [ocamlbuild] helpers. *)
module OCamlbuild : sig

  (** {1:cmd Command} *)

  val cmd : Cmd.t
  (** [cmd] is a command for [ocamlbuild] looked up using
      {!Topkg.Env.OCaml.tool}[ "ocamlbuild" `Build_os]. *)

  (** {1 Packages} *)

   val package_tags : ?roots:bool -> Fpath.t -> (String.set, R.msg) result
   (** [packages ~roots f] is the set of packages identifiers
       mentioned by the
       {{:https://github.com/gasche/manual-ocamlbuild/blob/master/manual.md#intro-ocamlfind}
       package tags} of the [_tags] file [f]. If [roots] is [true]
       (defaults to [false]) only root packages, i.e. the identifier
       before the first ['.'], are in the set.

       {b Warning.} This is a very dumb parsing that simply looks up
       for all ["package($ID)"] patterns in the [_tags] file. *)
end

(** [ocamlfind] helpers. *)
module OCamlfind : sig

  (** {1:cmd Command} *)

  val cmd : Cmd.t
  (** [cmd] is a command for [ocamlfind] looked up using
      {!Topkg.Env.OCaml.tool}[ "ocamlfind" `Build_os]. *)

  (** {1 Packages} *)

  val base_packages : String.set
  (** [base_packages] are the OCamlfind packages that are
       distributed with OCaml. *)
end

(** WWW browser interaction.

    {b Note.} Trying to load and reload URIs from the command line in
    a consistant manner across browsers and operating systems seems to
    be a hopeless endeavour. In particular the reload strategy
    mentioned below—useful to write API documentation—is
    an indication of what {e should} be done for what is believed to be the
    best user experience. But don't expect
    this work in all contexts (currently it only fully works with
    Chrome on Darwin and it is not even glitchless). If you know how
    to improve or extend the support for particular browsers and platforms
    get in touch {{:https://github.com/dbuenzli/topkg/issues/20}here}. *)
module Browser : sig

  (** {1 Browser} *)

  (** Command line interface. *)
  module Cli : sig

    (** {1 Command line} *)

    val browser : Cmd.t option Cmdliner.Term.t
    (** A [--browser] option and [BROWSER] environment variable to
        use with the [browser] argument of {!reload}. *)

    val prefix : bool Cmdliner.Term.t
    (** A [--prefix] option to use with the [prefix] argument of {!reload}. *)

    val background : bool Cmdliner.Term.t
    (** A [--background] option to use with [background] argument of
        {!reload}. *)
  end

  val reload :
    ?background:bool ->
    ?prefix:bool -> ?browser:Cmd.t -> uri:string -> (unit, R.msg) result
  (** [reload ~background ~prefix ~browser uri] tries to reload the URI
      [uri] or an URI prefixed by [uri] if prefix is [true] (defaults to
      [false]) in browser [browser].

      The reload should always lead to the reload of a single tab
      found as follows.
      {ol
      {- Repeat from the frontmost browser window to the backmost one until
         a tab to reload is found:
        {ol
        {- If the window's current tab's URI is [uri] (or is prefixed by [uri]
           when  [prefix] is [true]), reload this tab.}
        {- If the window has one or more tab whose URI is [uri] (or is prefixed
         by [uri] when [prefix] is [true]), pick the left most one, make it
         current in the window and reload it.}}}
      {- If no tab was found, get the frontmost window. If the current tab
         has no URI, use that tab with [uri] otherwise create a new tab
         with [uri] and make it current for the window.}}
      If [background] is [true] (defaults to [false]), the browser application
      should be kept in the background, only the reload should occur. If [false]
      the browser application and reloaded window should be brought into
      focus. *)
end

(** Command line pager interaction. *)
module Pager : sig

  (** {1 Pager} *)

  val find : unit -> (Cmd.t option, R.msg) result
  (** [find ()] is a pager command. First consults the [PAGER] environment
      variable, then tries [less] or [more] in that order. If the [TERM]
      environment variable is ["dumb"] unconditionnaly returns [None]. *)
end

(** Command line editor interaction. *)
module Editor : sig
  val edit_file : Fpath.t -> (int, R.msg) result
end

(** {1 Package care} *)

(** IPC with package description files  *)
module Ipc : sig

  (** {1 Asking packages} *)

  val ocaml : Cmd.t
  (** [ocaml] is a command for [ocaml] looked up using
      {!Topkg.Env.OCaml.tool}[ "ocaml" `Build_os]. *)

  val ask : pkg_file:Fpath.t -> 'a Topkg.Private.Ipc.t -> ('a, R.msg) result
  (** [ask pkg_file ipc] performs the IPC [ipc] with the package description
      file [pkg_file] using the interpreter {!ocaml}. *)
end

(** Package standard files. *)
module Std_files : sig

  (** {1 Standard files.} *)

  val of_pkg_file :
    pkg_file:Fpath.t -> (Topkg.Private.Std_files.t, [`Msg of string]) result
  (** [of_pkg_file ~pkg_file] are the standard files of the package description
      [pkg_file]. *)

  val change_log :
    Topkg.Private.Std_files.t -> (Fpath.t, [ `Msg of string ]) Rresult.result
  (** [change_log std_files] is [std_files]'s change log. *)

  val find_opam_file :
    Topkg.Private.Std_files.t -> (Fpath.t, [ `Msg of string ]) Rresult.result
    (** [find_opam_file std_files] is the first file mentioned in
        {!Topkg.Private.Std_files.opam} or [Fpath.v "opam"]. *)
end

(** Package linter.

    See also {!Topkg.Pkg.lint}.

    {b Warning.} The following functions log test results with
    {!Logs.App} level. *)
module Lint : sig

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
end

(** Distribution creation. *)
module Distrib : sig

  (** {1 Ustar archives} *)

  val tar :
    Fpath.t -> exclude_paths:Topkg.fpath list -> root:Fpath.t -> mtime:int ->
    (string, R.msg) result
  (** [tar dir ~exclude_paths ~root ~mtime] is a (us)tar archive that
      contains the file hierarchy [dir] except the relative
      hierarchies present in [exclude_paths]. In the archive, members
      are rerooted at [root] and sorted according to
      {!Fpath.compare}. They have their modification time set to
      [mtime] and file permission mode preserved. No other file
      metadata is preserved. *)

  (** {1 Bzip2 compression} *)

  val ensure_bzip2 : unit -> (unit, R.msg) result
  (** [ensure_bzip2 ()] makes sure the [bzip2] utility is available. *)

  val bzip2 : string -> dst:Fpath.t -> (unit, R.msg) result
  (** [bzip2 s dst] compresses [s] to [dst] using bzip2. *)

  (** {1 Distribution} *)

  type det
  (** The type for distribution determination. *)

  val build_dir : det -> Fpath.t
  (** [build_dir d] is the build directory to use. *)

  val name : det -> string
  (** [name d] is the package name to use. *)

  val commit_ish : det -> Topkg.Vcs.commit_ish
  (** [commit_ish d] is the commit-ish to base the distribution on. *)

  val version : det -> string
  (** [version d] is the version string to use. *)

  val rootname : ?opam:bool -> det -> Fpath.t
  (** [rootname ~opam d] is the root name for the distribution.
      It is is made from {!name} and {!version} glued
      together with a separator that depends on [opam] (defaults
      to [false]). A leading ['v'] character in the version is chopped
      before the elements are glued. *)

  val determine :
    pkg_file:Fpath.t -> build_dir:Topkg.fpath option ->
    name:string option -> commit_ish:Topkg.Vcs.commit_ish option ->
    version:string option -> (det, R.msg) Result.result
  (** [determine pkg_file ~build_dir ~name ~commit_ish ~version]
      determines a distribution for the package described by [pkg_file].
      [build_dir], [name], [commit_ish], [version] can be used to
      override the package description information. *)

  val clone_repo : det -> Topkg.Vcs.t -> (Fpath.t, R.msg) result
  (** [clone_repo det r] clones the repository [r] according to [det]
      inside a build directory and returns the directory of the clone. *)

  val prepare_repo :
    det -> dist_pkg_file:Fpath.t -> dir:Fpath.t ->
    (int * Topkg.fpath list, R.msg) result
  (** [prepare_repo det ~dir ~dist_pkg_file] prepares the distribution
      determined by [det] using the package description file
      [dist_pkg_file] (relative to [dir]) in an already freshly cloned repo
      [dir].

      This checkouts in [dir] the commit-ish specified by [det],
      performs distribution preparation (watermarks and massage hook)
      and returns the mtime to use for archiving and the list of paths
      to exclude from the archive expressed relative to [dir]. *)

  val archive_path : det -> Fpath.t
  (** [archive_path det] is the path to the archive of the distribution
      determined by [det]. *)

  val archive :
    det -> keep_dir:bool -> dir:Fpath.t -> exclude_paths:Topkg.fpath list ->
    mtime:int -> (Fpath.t, R.msg) result
  (** [archive ~keep_dir ~dir ~exclude_paths ~mtime det] archives the file paths
      of [dir] according to [det]. The path hierarchies in [exclude_paths]
      are excluded and [mtime] the POSIX timestamp mtime in seconds used
      as the modification time for all the archived paths.
      The resulting path to the archive is returned. If [keep_dir] is
      [false], [dir] is deleted if the archival is sucessfull. *)

  val unarchive : ?clean:bool -> Fpath.t -> (Fpath.t, R.msg) result
  (** [unarchive ~clean ar] unarchive [ar] in the same directory and
      returns the directory of the distribution. If [clean] is [true]
      (defaults to [false]) first deletes a distribution directory with
      the same name if it exists. *)
end

(** Build packages. *)
module Build : sig
  val pkg :
    pkg_file:Fpath.t -> dir:Fpath.t -> args:string list ->
    out:(OS.Cmd.run_out -> ('a, R.msg) result) -> ('a, R.msg) result
  (** [pkg ~pkg_file ~dir] builds the package in [dir] using the
      package file [pkg_file] (realtive to [dir]) and the arguments [args].
      Returns the exit code of the build. *)
end

(** Build and publish package documentation *)
module Doc : sig

  val publish_in_git_branch :
    branch:string -> name:string -> version:string -> docdir:Fpath.t ->
    dir:Fpath.t -> (unit, R.msg) result
    (** [publish_in_git_branch ~branch ~name ~version ~docdir ~dir]
        publishes the documentation directory [docdir] of a package
        named [name] at version [version] by replacing the [dir]
        sub-directory of the branch [branch] of the current working
        directory git repository (use ["."] to copy the docdir at the
        root directory of the branch).

        {b Note.} The publication procedure first checkouts the [gh-pages]
        in a temporary clone located in the {!Fpath.parent} directory
        of [docdir]. The [branch] branch of this clone is then pushed to
        the current working git repository, whose [branch] branch is then
        pushed to the remote repository. *)
end

(** Package delegate.

    {1 Delegate} *)
module Delegate : sig

  type t
  (** The type for delegates. *)

  val find :
    pkg_file:Fpath.t -> opam:Fpath.t option -> del:string option ->
    (t option, R.msg) result
  (** [find opam del] looks up a delegate according to the delegate lookup
      procedure, see [topkg-delegate(7)] for more details. If [del] is
      [Some d], returns [d] without consulting [pkg_file]. [opam] is
      the opam file to use for OPAM based delegate discovery, if
      unspecified looks up using [pkg_file]. *)

  val pp_not_found : unit Fmt.t
  (** [pp_not_found ppf ()] indicates that the delegate was not found
      on [ppf]. *)

  (** {1 Publish} *)

  val publish_distrib :
    del:t -> name:string -> version:string -> msg:string -> archive:Fpath.t ->
    (int, R.msg) Result.result
  (** [publish_distrib ~name ~version ~msg ~archive] publishes the distribution
      archive [archive] for the package named [name] with version [version]
      and publication message [msg]. *)

  val publish_doc :
    del:t -> name:string -> version:string -> msg:string -> docdir:Fpath.t ->
    (int, R.msg) Result.result
  (** [publish_distrib ~name ~version ~msg ~archive] publishes the documentation
      directory [docdir] for the package named [name] with version [version]
      and publication message [msg]. *)

  val publish_alt :
    del:t -> kind:string -> name:string -> version:string -> msg:string ->
    archive:Fpath.t -> (int, R.msg) Result.result
  (** [publish_alt ~kind ~name ~version ~msg ~archive] publishes the
      artefact [kind] for distribution archive [archive] of the package
      named [name] with ersion [version] and publication message [msg]. *)

  (** {1 Issues} *)

  val issue_list : del:t -> (int, R.msg) result
  (** [issue_list ~del] outputs the issue list on stdout and returns
      the delegate's exit code. *)

  val issue_show : del:t -> id:string -> (int, R.msg) result
  (** [issue_show ~del ~id] outputs information about issue [id] on
      stdout and returns the delegate's exit code. *)

  val issue_open : del:t -> title:string -> body:string -> (int, R.msg) result
  (** [issue_open ~del ~title ~body] create a new issue with title [title]
      and description body [body] and returns the delegate's exit code. *)

  val issue_close : del:t -> id:string -> msg:string -> (int, R.msg) result
  (** [issue_close ~del ~id ~msg] closes issue [id] with message [msg]. *)
end


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
