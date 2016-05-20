(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** The transitory OCaml package builder.

    See the {{!basics}basics}.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Preliminaries}

    In most cases you won't need this, jump directly
    to {{!Pkg}package description API}. *)

val ( >>= ) :
  ('a, 'b) Result.result -> ('a -> ('c, 'b) Result.result) ->
  ('c, 'b) Result.result
(** [r >>= f] is [f v] if [r = Ok v] and [r] if [r = Error _]. *)

val ( >>| ) : ('a, 'b) Result.result -> ('a -> 'c) -> ('c, 'b) Result.result
(** [r >>| f] is [Ok (f v)] if [r = Ok v] and [r] if [r = Error _]. *)

(** This definition re-export [result]'s constructors so that an
    [open Topkg] will get them in scope. *)
type ('a, 'b) r = ('a, 'b) Result.result = Ok of 'a | Error of 'b

type 'a result = ('a, [ `Msg of string]) r
(** The type for topkg results. *)

(** Result value combinators. *)
module R : sig

  (** {1 Errors} *)

  val reword_error : ('b -> 'c) -> ('a, 'b) Result.result ->
    ('a, 'c) Result.result
  (** [reword_error reword r] is:
      {ul
      {- [r] if [r = Ok v]}
      {- [Error (reword e)] if [r = Error e]}} *)

  (** {1 Error messages} *)

  type msg = [ `Msg of string ]
  (** The type for (error) messages. *)

  val error_msg : string -> ('b, [> msg]) Result.result
  (** [error_msg s] is [Error (`Msg s)]. *)

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> msg]) Result.result) format4 -> 'a
  (** [error_msgf fmt ...] is an error formatted according to [fmt]. *)

  val reword_error_msg :
    ?replace:bool -> (string -> msg) -> ('a, msg) Result.result ->
    ('a, [> msg]) Result.result
  (** [reword_error_msg ~replace reword r] is like {!reword_error} except
      if [replace] is [false] (default), the result of [reword old_msg] is
      concatened, on a new line to the old message. *)
end

val strf : ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf] is [Printf.asprintf]. *)

(** Strings. *)
module String : sig

  (** {1 String additions} *)

  include module type of String

  val head : string -> char option
  (** [head s] if [Some s.[0]] if [s <> ""] and [None] otherwise. *)

  (** {1:preds Predicates} *)

  val is_prefix : affix:string -> string -> bool
  (** [is_prefix ~affix s] is [true] iff [affix.[i] = s.[i]] for
      all indices [i] of [affix]. *)

  val is_suffix : affix:string -> string -> bool
  (** [is_suffix ~affix s] is true iff [affix.[n - i] = s.[m - i]] for all
      indices [i] of [affix] with [n = String.length affix - 1] and [m =
      String.length s - 1]. *)

  val for_all : (char -> bool) -> string -> bool
  (** [for_all p s] is [true] iff for all indices [i] of [s], [p s.[i]
      = true]. *)

  val exists : (char -> bool) -> string -> bool
  (** [exists p s] is [true] iff there exists an index [i] of [s] with
      [p s.[i] = true]. *)

  (** {1:subs Extracting substrings} *)

  val with_index_range : ?first:int -> ?last:int -> string -> string
  (** [with_index_range ~first ~last s] are the consecutive bytes of [s]
      whose indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and last to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string is
      returned. *)

  val cut : ?rev:bool -> sep:char -> string -> (string * string) option
  (** [cut ~sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the
      first match of the separator character [sep] or [None] if
      [sep] can't be matched in [s]. Matching starts from the
      beginning of [s] ([rev] is [false], default) or the end ([rev]
      is [true]).

      The invariant [l ^ (String.make 1 sep) ^ r = s] holds. *)

  val cuts : ?empty:bool ->  sep:char -> string -> string list
  (** [cuts ~sep s] is the list of all substring of [s] that are delimited by
      matches of [sep]. Empty substrings are ommited in the list if
      [empty] is [falsee] (defaults to [true]). The invariant
      [String.concat (String.make 1 sep) (split ~sep s) = s] holds. *)

  (** {1:vers Parsing version strings} *)

  val parse_version : string -> (int * int * int * string option) option
  (** [parse_version] parses version strings of the form:
{[
"[v]major.minor[.patchlevel][+additional-info]"
]}
      into [(major, minor, patch, additiona_info)] tuples. *)
end

type fpath = string
(** The type for file system paths. *)

(** File system paths *)
module Fpath : sig

  (** {1 File system paths} *)

  type t = fpath
  (** The type for file system paths. *)

  val append : t -> t -> t
  (** [append p q] appends [q] to [p] as follows:
      {ul
      {- If [q] is absolute then [q] is returned}
      {- Otherwise appends [q]'s segments to [p] using {!Filename.dir_sep}}} *)

  val ( // ) : t -> t -> t
  (** [p // q] is [append p q]. *)

  val is_dir_path : t -> bool
  (** [is_dir_path p] is [true] iff [p] represents a directory. This means
      that [p] is [.], [..] or ends with [/], [/..] or [/.]. *)

  val is_file_path : t -> bool
  (** [is_file_path p] is [not (is_dir_path true)]. *)

  val basename : t -> string
  (** [basename p] is [p]'s basename, the last non empty segment of [p]. *)

  val dirname : t -> string
  (** [dirname p] is [p]'s dirname, [p] without its  last non empty segment. *)

  (** {1 File extensions} *)

  val get_ext : t -> string
  (** [get_ext p] is [p]'s filename extension (including the ['.']) or
      the empty string if there is no extension *)

  val has_ext : string -> t -> bool
  (** [has_ext e p] is [true] iff [e] is a suffix of [p]. *)

  val rem_ext : t -> t
  (** [rem_ext p] is [p] without its filename extension. *)

end

(** Command lines.

    Both command lines and command line fragments using the same are
    represented with the same {{!t}type}.

    When a command line is {{!section:OS.Cmd.run}run}, the first
    element of the line defines the program name and each other
    element is an argument that will be passed {e as is} in the
    program's [argv] array: no shell interpretation or any form of
    argument quoting and/or concatenation occurs. *)
module Cmd : sig

  (** {1:lines Command line fragments} *)

  type t
  (** The type for command line fragments. *)

  val v : string -> t
  (** [v cmd] is a new command line (or command line fragment)
      whose first argument is [cmd]. *)

  val empty : t
  (** [empty] is an empty command line. *)

  val is_empty : t -> bool
  (** [is_empty l] is [true] iff [l] is empty. *)

  val ( % ) : t -> string -> t
    (** [l % arg] adds [arg] to the command line [l]. *)

  val ( %% ) : t -> t -> t
  (** [l %% frag] appends the line fragment [frag] to [l]. *)

  val add_arg : t -> string -> t
  (** [add_arg l arg] is [l % arg]. *)

  val add_args : t -> t -> t
  (** [add_args l frag] is [l %% frag]. *)

  val on : bool -> t -> t
  (** [on bool line] is [line] if [bool] is [true] and {!empty}
      otherwise. *)

  val p : fpath -> string
  (** [p] is [(fun f -> f)]. *)

  (** {1:predicates Predicates and comparison} *)

  val equal : t -> t -> bool
  (** [equal l l'] is [true] iff [l] and [l'] are litterally equal. *)

  val compare : t -> t -> int
  (** [compare l l'] is a total order on command lines. *)

  (** {1:convert Conversions and pretty printing} *)

  val to_list : t -> string list
  (** [to_list l] is [l] as a list of strings. *)

  val of_list : ?slip:string -> string list -> t
  (** [of_list ?slip l] is a command line from the list of arguments
      [l].  If [slip] is specified it is added on the command line
      before each element of [l]. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf cmd] formats an unspecified representation of [cmd] on
      [ppf]. *)
end

(** Topkg log. *)
module Log : sig

  (** {1 Reporting levels} *)

  (** The type for reporting levels. *)
  type level = App | Error | Warning | Info | Debug

  val level : unit -> level option
  (** [level ()] is the current reporting level. *)

  val set_level : level option -> unit
  (** [set_level l] sets the current reporting level to [l]. *)

  val level_to_string : level option -> string
  (** [level_to_string l] converts [l] to an unspecified human-readable
      US-ASCII string that can be parsed back by {!level_of_string}. *)

  val level_of_string : string -> (level option, [`Msg of string]) Result.result
  (** [level_of_string s] parses the representation of {!level_to_string}
      from [s]. *)

  (** {1 Log functions} *)

  type 'a msgf =
    (?header:string ->
     ('a, Format.formatter, unit) Pervasives.format -> 'a) -> unit

  val msg : level -> 'a msgf -> unit
  (** [msg l (fun m -> m fmt ...)] logs with level [l] a message formatted
      with [fmt]. *)

  val app : 'a msgf -> unit
  (** [app] is msg [App]. *)

  val err : 'a msgf -> unit
  (** [err] is [msg Error]. *)

  val warn : 'a msgf -> unit
  (** [err] is [msg Warning]. *)

  val info : 'a msgf -> unit
  (** [err] is [msg Info]. *)

  val debug : 'a msgf -> unit
  (** [err] is [msg Debug]. *)

  val on_error_msg : ?level:level -> use:(unit -> 'a) -> 'a result -> 'a
  (** [on_error_msg ~level r] is:
      {ul
      {- [v] if [r = Ok v]}
      {- [use e] if [r = Error (`Msg e)]. As a side effect [e] is logged
         with level [level] (defaults to {!Log.Error}).}} *)

  (** {1 Monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level [Warning]. *)

end

(** OS interaction. *)
module OS : sig

  (** {1 OS} *)

  (** Environment variables *)
  module Env : sig

    (** {1 Variables} *)

    val var : string -> string option
    (** [var name] is the value of the environment variable [name], if
        defined. *)

    val opt_var : string -> absent:string -> string
    (** [opt_var name ~absent] is the value of the optionally defined
        environment variable [name], if defined, and [absent] if undefined. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:paths Famous file paths} *)

    val null : fpath
    (** [null] represents a file on the OS that discards all writes
        and returns end of file on reads. *)

    val dash : fpath
    (** [dash] is ["-"]. This value is is used by {!read} and {!write}
        to respectively denote [stdin] and [stdout]. *)

    (** {1:exdel Existence and deletion} *)

    val exists : fpath -> bool result
    (** [exists file] is [true] if [file] is a regular in the file
        system and false otherwise. Symbolic links are followed. *)

    val must_exist : fpath -> fpath result
    (** [must_exist file] is [file] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are followed. *)

    val delete : ?must_exist:bool -> fpath -> unit result
    (** [delete ~must_exist file] deletes file [file]. If [must_exist]
        is [true] (defaults to [false]) an error is returned if [file]
        doesn't exist. *)

    (** {1:fold Folding over file hierarchies} *)

    val fold :
      ?skip:(fpath -> bool) -> (fpath -> 'a -> 'a) -> 'a ->
      fpath list -> 'a result
    (** [fold_files skip f acc paths] folds [f] over the {e files}
        found in the file hierarchies starting at [paths].  Files
        and directories [p] for which [skip p] is [true] are skipped.
        [skip] defaults to [(fun _ -> false)]. *)

    (** {1:rw Reading and writing} *)

    val read : fpath -> string result
    (** [read file] is [file]'s contents. If [file] is {!dash} reads
        from {!Pervasives.stdin} and the channel is not closed when
        the function returns. *)

    val write : fpath -> string -> unit result
    (** [write file content] writes [content] to [file]. If [file] is {!dash}
        writes to {!Pervasives.stdout} and flushes but doesn't close the channel
        when the function returns. *)

    (** {1:tmpfiles Temporary files} *)

    val tmp : unit -> fpath result
    (** [tmp ()] creates a temporary file and returns its name. The file
        is destroyed at the end of program execution. *)
  end

  (** Directory operations. *)
  module Dir : sig

    (** {1:exists Existence and contents} *)

    val exists : fpath -> bool result
    (** [exists dir] is [true] if directory [dir] exists in the file
        system. Symbolic links are followed. *)

    val must_exist : fpath -> fpath result
    (** [must_exist dir] is [dir] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are followed. *)

    val contents : ?dotfiles:bool -> ?rel:bool -> fpath -> fpath list result
    (** [contents ~dotfiles ~rel dir] is the list of directories and filse
        in [dir]. If [rel] is [true] (defaults to [false]) the resulting
        paths are relative to [dir], otherwise they have [dir] prepended.
        If [dotfiles] is [false] (default) elements that start with a [.]
        are omitted. *)

    (** {1:current Current working directory} *)

    val current : unit -> fpath result
    (** [current ()] is the current working directory. *)

    val set_current : fpath -> unit result
    (** [set_current dir] sets the current working directory to [dir]. *)
  end

  (** Running commands. *)
  module Cmd : sig

    (** {1:exists Command existence} *)

    val exists : Cmd.t -> bool result
    (** [exists cmd] is [true] if the executable of [cmd] can be found
        in the path and [false] otherwise. *)

    val must_exist : Cmd.t -> Cmd.t result
    (** [must_exist cmd] is [cmd] if the executable of [cmd] can be found
        in the path and an error otherwise. *)

    (** {1:run Running commands} *)

    val run : ?err:fpath -> Cmd.t -> unit result
    (** [run cmd] runs the command [cmd]. [std{i,o,err}] are connected
        to the invoking process' standard channels. If [err] is specified
        [stderr] is redirected to the given file (e.g. {!File.null}). *)

    val run_status : ?err:fpath -> Cmd.t -> [`Exited of int] result
    (** [run_status cmd] is like {!run}, but doesn't error on non-zero
        exit status. *)

    (** {1 Capturing standard output} *)

    type run_status = Cmd.t * [`Exited of int ]
    (** The type for run statuses, the command that was run and the run
        status. *)

    val success : ('a * run_status) result -> 'a result
    (** [success r] is:
        {ul
        {- [Ok v] if [r = Ok (v, (_, `Exited 0))]}
        {- [Error _] otherwise. Non [`Exited 0] statuses are turned into
           an error message.}} *)

    type run_out
    (** The type for representing the standard output of a command run. *)

    val out_string : ?trim:bool -> run_out -> (string * run_status) result
    (** [out_string ~trim o] captures the standard output [o] as a [string].
        If [trim] is [true] (default) the result is passed through
        {!String.trim}. *)

    val out_lines : ?trim:bool -> run_out -> (string list * run_status) result
    (** [out_lines ~trim] is like {!out_string} but the result is
        {{!String.cuts}cut} on newlines (['\n']). *)

    val out_file : fpath -> run_out -> (unit * run_status) result
    (** [out_file f o] writes the standard output [o] to file [f]. *)

    val out_stdout : run_out -> (unit * run_status) result
    (** [out_stdout o] redirects the standard output [o] to the current
        process standard output. *)

    val to_string : ?trim:bool -> run_out -> string result
    (** [to_string] is [(out_string ?trim o |> success)]. *)

    val to_lines : ?trim:bool -> run_out -> string list result
    (** [to_lines ?trim o] is [(out_string ?trim o |> success)]. *)

    val to_file : fpath -> run_out -> unit result
    (** [to_file f o] is [(out_file f o |> success)] *)

    val run_out : ?err:fpath -> Cmd.t -> run_out
    (** [run_out cmd] represents the standard output of the command run [cmd].
        [std{i,err}] are connected to the invoking prcoess stream and standard
        output can be consumed with {!to_string}, {!to_lines} or {!to_file}.
        If [err] is specified [stderr] is redirected to the given file. *)
  end
end

(** Version control system repositories. *)
module Vcs : sig

  (** {1:vcsops Version control system repositories} *)

  type kind = [ `Git | `Hg ]
  (** The type for version control systems (VCS). *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on [ppf]. *)

  type commit_ish = string
  (** The type for symbols resolving to a commit. The module uses
      ["HEAD"] for specifying the current checkout; use
      this symbol even if the underlying VCS is [`Hg]. *)

  type t
  (** The type for version control systems repositories. *)

  val kind : t -> kind
  (** [kind r] is [r]'s VCS kind. *)

  val dir : t -> fpath
  (** [dir r] is [r]'s repository directory. *)

  val cmd : t -> Cmd.t
  (** [cmd r] is the base VCS command to use to act on [r].

      {b Warning} Prefer the functions below to remain VCS independent. *)

  val find : ?dir:fpath -> unit -> t option result
  (** [find ~dir ()] looks for a VCS repository in working directory [dir]
      (not the repository directory like [.git], default is guessed
       automatically). *)

  val get : ?dir:fpath -> unit -> t result
  (** [get] is like {!find} but returns an error if no VCS was found. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf r] prints an unspecified representation of [r] on [ppf]. *)

  (** {1:state Repository state} *)

  val is_dirty : t -> bool result
  (** [is_dirty r] is [Ok true] iff the working tree of [r] has uncommited
      changes. *)

  val not_dirty : t -> unit result
  (** [not_dirty] is [Ok ()] iff the working directory of [r] is not dirty and
      an error that enjoins to stash or commit otherwise. *)

  val file_is_dirty : t -> fpath -> bool result
  (** [file_id_dirty r f] is [Ok true] iff [f] has uncommited changes. *)

  val head : ?dirty:bool -> t -> string result
  (** [head ~dirty r] is the HEAD commit identifier of the repository
      [r]. If [dirty] is [true] (default), and indicator is appended to
      the commit identifier if the working tree of [r] {!is_dirty}. *)

  val commit_id : ?dirty:bool -> ?commit_ish:commit_ish -> t -> string result
  (** [commit_id ~dirty ~commit_ish r] is the object name (identifier)
      of [commit_ish] (defaults to ["HEAD"]). If [commit_ish] is
      ["HEAD"] and [dirty] is [true] (default) and indicator is
      appended to the identifier if the working tree is dirty. *)

  val commit_ptime_s : ?commit_ish:commit_ish -> t -> int result
  (** [commit_ptime_s t ~commit_ish] is the POSIX time in seconds
      of commit [commit_ish] (defaults to ["HEAD"]) of repository [r]. *)

  val describe : ?dirty:bool -> ?commit_ish:commit_ish -> t -> string result
  (** [describe ~dirty ~commit_ish r] identifies [commit_ish] (defaults to
      ["HEAD"]) using tags from the repository [r]. If [commit_ish] is
      ["HEAD"] and [dirty] is [true] (default) an indicator is appended to
      the identifier if the working tree is dirty. *)

  val tags : t -> string list result
  (** [tags r] is the list of tags in the repo [r]. *)

  val changes :
    ?until:commit_ish -> t -> after:commit_ish -> (string * string) list result
  (** [changes r ~after ~until] is the list of commits with their
      one-line message from commit-ish [after] to commit-ish [until]
      (defaults to ["HEAD"]). *)

  val tracked_files : ?tree_ish:string -> t -> fpath list result
  (** [tracked_files ~tree_ish r] are the files tracked by the tree object
      [tree_ish] (defaults to ["HEAD"]). *)

  (** {1:ops Repository operations} *)

  val clone : t -> dir:fpath -> unit result
  (** [clone r ~dir] clones [r] in directory [dir]. *)

  val checkout : ?branch:string -> t -> commit_ish:commit_ish -> unit result
  (** [checkout r ~branch commit_ish] checks out [commit_ish]. Checks out
      in a new branch [branch] if provided. *)

  val commit_files : ?msg:string -> t -> fpath list -> unit result
  (** [commit_files r ~msg files] commits the file [files] with message
      [msg] (if unspecified the VCS should prompt). *)

  val tag :
    ?force:bool -> ?sign:bool -> ?msg:string -> ?commit_ish:string -> t ->
    string -> unit result
  (** [tag r ~force ~sign ~msg ~commit_ish t] tags [commit_ish] with [t]
      and message [msg] (if unspecified the VCS should prompt).  if
      [sign] is [true] (defaults to [false]) signs the tag ([`Git] repos only).
      If [force] is [true] (default to [false]) doesn't fail if the tag
      already exists. *)

  val delete_tag : t -> string -> unit result
  (** [delete_tag r t] deletes tag [t] in repo [r]. *)
end

(** {1 Package description} *)

(** Build configuration. *)
module Conf : sig

  (** {1:tool Tool lookup}

      If your package description needs to run tools (e.g. in the
      pre and post build hooks, see {!Pkg.build}) it should get the
      tool to invoke with the following function. This allows to
      control the executable being run which is useful for
      cross-compilation scenarios. *)

  type os = [ `Build_os | `Host_os ]
  (** The type for operating systems.
      {ul
      {- [`Build_os] is the build OS, the OS on which the package is built.}
      {- [`Host_os] is the host OS, the OS on which the package is hosted
         and runs.}} *)

  val tool : string -> os -> Cmd.t
  (** [tool cmd os] is a command [cmd] which can be run on the build OS
      which produces output suitable for the OS [os].

      The actual command returned depends on the following lookup procedure,
      here exemplified on the invocation [tool "mytool" `Host_os] (resp.
      [`Build_os]).
      {ol
      {- [Cmd.v "cmd"], if the environment variable HOST_OS_MYTOOL (resp.
         BUILD_OS_MYTOOL) is set to ["cmd"]}
      {- [Cmd.v (Fpath.append path "mytool")], if the environment variable
         HOST_OS_XBIN  (resp. BUILD_OS_BIN) is set to [path].}
      {- [Cmd.v ("mytool" ^ "suff")], if the environment variable
         HOST_OS_SUFF (resp. BUILD_OS_SUFF).}
      {- [Cmd.(v "ocamlfind" % "mytool")] if ["mytool"] is part of
         the OCaml tools that can be invoked through ocamlfind.}
      {- [Cmd.v "mytool"] otherwise.}} *)

  (** {1:kconv Key value converters} *)

  type 'a conv
  (** The type for key value converters. *)

  val conv :
    ?docv:string -> (string -> 'a result) -> (Format.formatter -> 'a -> unit) ->
    'a conv
  (** [conv ~docv parse print] is a configuration value converter
      parsing values with [parse] and printing them with
      [print]. [docv] is a documentation meta-variable used in the
      documentation to stand for the configuration value, defaults to
      ["VALUE"]. *)

  val bool : bool conv
  (** [bool] is a converter for booleans. *)

  val int : int conv
  (** [int] is a converter for integers. *)

  val string : string conv
  (** [string] is a converter for strings. *)

  val fpath : fpath conv
  (** [fpath] is a converter for file paths. *)

  val some : ?none:string -> 'a conv -> 'a option conv
  (** [some conv] is like [conv] but wraps the parsed value in [Some].
      [none] is the string printed for [None] by the converter printer,
      defaults to [""]. *)

  (** {1:key Keys}

      {b Warning.} Configuration keys must be created before the call
      to {!Pkg.describe}. Yes you are right, that's a little bit dirty. *)

  type 'a key
  (** The type for configuration keys whose lookup value is of type ['a].

      A configuration key has a name and represents a value of type
      ['a] in a build configuration. If ["name"] is the name of the key
      then its value can be specified on the command line using
      [--name v] where [v] is the value of the key and is parsed
      according to the key's {{!conv}value converter}.

      There are a few predefined key, see the {{!conf}configuration section}. *)

  val key :
    ?docv:string -> ?doc:string -> ?env:string -> string -> 'a conv ->
    absent:'a -> 'a key
  (** [key name conv ~absent ~env ~doc ~docv] is a configuration key
      with name [name] parsed from the command line with [conv].
      [absent] is used if the value is not specified on the command
      line. If [env] is specified and exists in the process environment,
      the value of the variable is parsed with [conv] and used instead
      of [absent] when needed.

      [doc] is a documentation string for the key. [docv] is a meta
      is a documentation to stand for the key value, defaults to the
      [docv] of [conv]. In [doc] occurences of the substring $(docv)
      are replaced by the value of [docv]. *)

  val discovered_key :
    ?docv:string -> ?doc:string -> ?env:string -> string -> 'a conv ->
    absent:(unit -> 'a result) -> 'a key
  (** [discovered_key] is like {!key} but the absent value is discovered,
      {e if needed}, with [discover]. *)

  val with_pkg : ?default:bool -> string -> bool key
  (** [with_pkg ~default pkg] is a boolean configuration key named
      [(strf "with-%s" pkg)] to assert existence of OPAM packages.
      If absent defaults to [default].

      Usually specified in OPAM build instructions with:
      {["--with-thatpkg" "%{thatpkg:installed}%"]} along with an entry in the
      depopt field of the OPAM file.

      {b Warning.} Only use this combinator for denoting OPAM
      package existence, the resulting key may switch to a discovery
      process in the future. *)

  (** {1:conf Configurations} *)

  type t
  (** The type for configurations. *)

  val value : t -> 'a key -> 'a
  (** [value c k] is the value of configuration key [k] in [c].

      @raise Invalid_argument  If [k] was (illegaly) created after the call
      to {!Pkg.describe} or if dirty tricks are being played. *)

  val pkg_name : t -> string
  (** [pkg_name c] is either the value of the package name as given to
      {!Pkg.describe} or the value of a predefined key [--pkg-name] which
      overrides the package name. This defines the name of the generated
      OPAM install file. Used to handle {{!multiopam}multiple
      OPAM packages}. *)

  val build_dir : t -> fpath
  (** [build_dir c] is either the value of build directory as given
      to {!Pkg.describe} via {!Pkg.build} or the value of a predefined
      key [--build-dir] which overrides the package definition. *)

  val vcs : t -> bool
  (** [vcs c] is the value of a predefined key [--vcs].
      It is [true] if the package directory is VCS managed. Usually
      should not be specified manually: if absent it is determined
      automatically by using {!Topkg.Vcs.find} and used to determine
      the {!build_context}. *)

  val installer : t -> bool
  (** [installer c] is the value of a predefined key [--installer].
      It is [true] if the build is initiated by an installer like OPAM.
      If absent defaults to [false]. Usually specified in OPAM build
      instructions with ["--installer" "true"]. This is used to
      determine the {!build_context}. *)

  type build_context = [`Dev | `Distrib | `Pin ]
  (** The type for build contexts. See {!val:build_context} for semantics. *)

  val build_context : t -> [`Dev | `Distrib | `Pin ]
  (** [build_context c] is the build context of [c]. This is derived from
      {!vcs} and {!installer} as follows.
      {ul
      {- [`Distrib] iff [not (vcs c)]. No VCS is present, this is a build from
         a distribution. If there are configuration bits they should
         be setup according to the build configuration.}
      {- [`Dev] iff [vcs c && not (installer c)]. This is a development
         build invoked manually in a source repository. The repository checkout
         should likely not be touched and configuration bits not be setup.
         This is happening for example if the developer is testing the package
         description in her working source repository by invoking
         [pkg/pkg.ml] or [topkg build].}
      {- [`Pin] iff [vcs c && installer c]. This is a package manager pin build.
         In this case the repository checkout may need to be massaged
         into a pseudo-distribution for the package to be installed. This means
         that distribution watermarking and massaging should be performed,
         see {!Pkg.distrib} and the [prepare_on_pin] argument of {!Pkg.build}.
         Besides exisiting configuration bits should be setup according to the
         build environment.}} *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf c] formats an unspecified representation of [c] on [ppf]. *)

  (** {1:ocaml OCaml configuration} *)

  (** OCaml configuration. *)
  module OCaml : sig

    (** {1:conf OCaml system configuration} *)

    type conf = t

    type t
    (** The type for OCaml configurations. *)

    val v : conf -> os -> t
    (** [v c os] is the configuration of the OCaml system for the OS
        [os] obtained by reading the output of [tool "ocamlc" os] with
        the [-config] option. *)

    val find : string -> t -> string option
    (** [find k c] looks up key [k] in configuration [c]. *)

    val version : t -> int * int * int * string option
    (** [version] is the compiler version string
        ["major.minor[.patchlevel][+additional-info]"] parsed into
        [(major, minor, patch, additional-info)]. *)

    val ext_asm : t -> string
    (** [ext_asm] is the file extension for assembly files, e.g. [".s"]. *)

    val ext_obj : t -> string
    (** [ext_asm] is the file extension for C object files, e.g. [".o"]. *)

    val ext_lib : t -> string
    (** [ext_lib] is the file extension for C static libraries, e.g. [".a"]. *)

    val ext_dll : t -> string
    (** [ext_dll] is the file extension for C dynamically linked libraries,
        e.g. [".so"]. *)

    val ext_exe : t -> string
    (** [ext_exe] is the file extension for binary executables, e.g. [".exe"]
        or [""]. Until at least OCaml 4.03 this information is not readily
        available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        discovered as described in
        {{:http://lists.ocaml.org/pipermail/wg-windows/2015-July/000037.html}
        this message}. *)

    val native : t -> bool
    (** [native] is [true] if native compilation (i.e. [ocamlopt]) is
        available. Until at least OCaml 4.03 this information is not
        readily available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        [true] is returned iff the standard library directory has the
        [libasmrun] C static library. *)

    val native_dynlink : t -> bool
    (** [native_dynlink] is [true] if native dynamic linking is
        available.  Until at least OCaml 4.03 this information is not
        readily available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        [true] is returned iff the standard library directory has the
        [dynlink.cmxa] library. *)

    val dump : Format.formatter -> t -> unit
    (** [dump ppf c] prints an unspecified representation of [c] on [ppf]. *)
  end
end

(** Exts defines sets of file extensions. *)
module Exts : sig

  (** {1 File extensions} *)

  type ext
  (** The type for file extensions. *)

  type t = ext list
  (** The type for lists of file extensions. *)

  val api : t
  (** [api] is [exts [".mli"; ".cmi"; ".cmti"; ".cmx"]] *)

  val cmx : ext list
  (** [cmx] is [ext ".cmx"] *)

  val c_library : ext list
  (** [c_library] is the extension for C libraries (archives). The
      actual value is determined from {{!Conf.OCaml}OCaml's configuration}. *)

  val c_dll_library : ext list
  (** [c_dll_library] is the extension for C dynamic libraries (archives). The
      actual value is determined from {{!Conf.OCaml}OCaml's configuration}. *)

  val library : ext list
  (** [library] is [exts [".cma"; ".cmxa"; ".cmxs"] @ c_library] *)

  val module_library : ext list
  (** [module_library] is [(api @ library)]. *)

  val exe : ext list
  (** [exe] is the extension for executables. The actual value is determined
      from {{!Conf.OCaml}OCaml's configuration}. *)

  val exts : string list -> ext list
  (** [exts ss] is [ss] as a list of extensions. *)

  val ext : string -> ext list
  (** [ext s] is [s] as a list of extensions. *)

  (**/**)
  val ext_to_string : Conf.OCaml.t -> ext -> string
  (**/**)
end

(** Package description.

    See the {{!basics}basics}. *)
module Pkg : sig

  (** {1:install Installation description}

      The installation description generates an OPAM install file
      which is simply a description of file moves (in the [mv] sense)
      from the build or source directory to standard install
      directories. Describing these moves in a given build
      configuration effectively determines what needs to built by the
      {{!build}package build command}. *)

  type install
  (** The type for representing a set of install moves. *)

  type field =
    ?force:bool -> ?built:bool -> ?cond:bool -> ?exts:Exts.t -> ?dst:fpath ->
    fpath -> install
  (** The type for an install field, a function that describe file
      moves to a particular installation directory. In the simplest form
      a call [field src] simply moves the file [src] at the root of the
      install directory of the field.

      In general [field ~force ~built ~cond ~exts ~dst src] generates install
      move as follows:
      {ul
      {- [dst] is the path were the source is written to. Expressed
         relative to the install directory of the field. Defaults
         to [Fpath.basename src], i.e. at the root of the install directory.
         If [dst] is a {{!Fpath.is_dir_path}directory path}, the destination
         is [(dst ^ Fpath.basename src)].}
      {- If [exts] is present and non empty, generates the list of
         paths [List.map (fun e -> src ^ e)] and a move for
         each of these. For example [field ~exts:]{!Exts.api}[ "src/m"] would
         generate a move for the files ["src/m.mli"], ["src/m.cmi"],
         ["src/m.cmti"], ["src/m.cmx"].}
      {- If [cond] is [false] (defaults to [true]) no file move is generated.
         This provides a convenient way to conditionalize installation based
         on the build configuration for example:
      {[let has_jsoo = Conf.value jsoo in
Pkg.lib ~cond:has_jsoo ~exts:Exts.module_library "src/mylib_jsoo"
      ]}}
      {- If [built] is [true] (default), [src] is expressed relative
         to the {{!build}build directory} of the distribution and the path
         [src] will be given to {{!build}build system invocation}
         for construction.
         If [false], [src] is relative to the root of the distribution
         and is excluded from the build system invocation; this can
         be used for installing files that don't need to be built.}
      {- If [force] is [true] (defaults to [false]) it disables the automatic
         [src] filtering performed by the library. When [false],
         the library will automatically disable certain build artefact
         depending on {{!Conf.OCaml}OCaml's configuration}, one such
         example is filtering native code build artefact if the OCaml install
         doesn't support
         {{!Conf.OCaml.native}native code compilation}}} *)

  type exec_field = ?auto:bool -> field
  (** The type for field that install.

      TODO reword this and make more precise. If [auto] is [true],
      selects the native binary over the byte code one according to
      the value of {!Conf.OCaml.native} and adds {!Exts.exe} to the
      destination (which does the right thing on Windows). *)

  val bin : exec_field
  (** [bin] is a field that installs to a common [bin/] directory. *)

  val doc : field
  (** [doc] is a field that installs to a package specific [doc/]
      directory *)

  val etc : field
  (** [etc] is a field that installs to a package specific [etc/]
      directory. *)

  val lib : field
  (** [lib] is a field that installs to a package specific [lib/]
      directory. *)

  val libexec : exec_field
  (** [libexec] is a field that installs to a package specific [lib/]
      directory but with the executable bit set. *)

  val man : field
  (** [man] is a field that installs to a common [man/] directory. See
      the {{:https://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
      OPAM manual} for details. *)

  val misc : field
  (** [misc] is a field that installs to an arbitrary absolute path,
      the user will be prompted for authorization,
      see the {{:https://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
      OPAM manual} for details. *)

  val sbin : exec_field
  (** [sbin] is a field that installs to a common [sbin/] directory. *)

  val share : field
  (** [share] is a field that installs to a package specific [share/]
      directory. *)

  val share_root : field
  (** [share_root] is a field that installs to a common [share/]
      directory. *)

  val stublibs : field
  (** [stublibs] is a field that install to a common [lib/stublibs/]
      directory. Used for OCaml C stub directory. *)

  val toplevel : field
  (** [toplevel] is a field that installs to a common [lib/toplevel/]
      directory. *)

(**/**)
  val unknown : string -> field
(**/**)

  (** {2 Higher-level installs} *)

  val mllib :
    ?field:field -> ?cond:bool -> ?api:string list -> ?dst_dir:fpath ->
    fpath -> install
  (** [mllib ~field ~cond ~api ~dst_dir mllib] installs an OCaml library
      described by the
      {{:https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc#19-archives-documentation}OCamlbuild .mllib file} [mllib] with:
      {ul
      {- [field] is the field where it gets installed (defaults to {!lib}).}
      {- If [cond] is [false] (defaults to [true]), no move is generated.}
      {- [api] is the list of modules that defines the public interface
         of the library, if [None] all the modules mentioned in [mllib]
         are part of the public interface.}
      {- [dst_dir] is the destination directory of the library
         in the field. If unspecified this is the root of the field's
         directory.}} *)

  (** {1:build Build description} *)

  type build
  (** The type for package build description. *)

  val build :
    ?prepare_on_pin:bool ->
    ?dir:fpath ->
    ?pre:(Conf.t -> unit result) ->
    ?cmd:(Conf.t -> Conf.os -> Cmd.t) ->
    ?post:(Conf.t -> unit result) -> unit -> build
  (** [build ~prepare_on_pin ~dir ~cmd ~pre ~post] describes the package
      build procedure.
      {ul
      {- [prepare_on_pin] if [true] (default) distribution
         preparation is performed if a [`Pin]
         {{!Conf.build_context}build context} is detected. This means that
         the checkout is watermarked and the massage hook is invoked,
         see step 2. of {{!distdetails}distribution creation}.}
      {- [dir] is the directory where build artefacts are generated,
         (defaults to ["_build"]). Note that his value can be overriden
         from the command line.}
      {- [pre] is a hook that is invoked with the build context, after
         distribution preparation if applicable, but before the build
         command. It can be used to adapt the build setup according to
         the build configuration. Default is a nop.}
      {- [cmd] determines the build command that will be invoked with the
         targets to build as determined by {{!install}install} moves.
         It is given the build configuration an {{!Conf.os}OS specification}
         and and must return a command line to run that will buid the targets
         in the {{!Conf.build_dir}build directory} of the build configuration.
         Defaults to:
{[
fun c os ->
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  Cmd.(ocamlbuild % "-use-ocamlfind" % "-classic-display" %
       "-build-dir" % build_dir)
]}}
      {- [post] is a hook that is invoked with the build context after
         the build command returned sucessfully. Default is a nop.}}

      {b Warning.} If you are invoking tools in your hooks consider
      using {!Conf.tool} to look them up it helps for cross-compilation. *)

  (** {1:distrib Distribution description} *)

  type watermark = string * [ `String of string | `Version | `Name
                            | `Vcs of [`Commit_id]
                            | `Opam of fpath option * string * string]
  (** The type for watermarks. A watermark identifier, e.g. ["ID"] and its
      definition:
      {ul
      {- [`String s], [s] is the definition.}
      {- [`Name], is the name of package.}
      {- [`Version], is the version of the distribution.}
      {- [`Vcs `Commit_id], is the commit identifier (hash) of the
         distribution. May be post-fixed by ["dirty"] in {!Env.build}
         pin builds.}
      {- [`Opam (file, field, sep)], is the values of the field
         [field] concatenated with separator [sep] of the OPAM file
         [file], expressed relative to the source repository, if
         [file] is [None] this is the current package's OPAM file. Not
         all fields are supported see the value of
         {!Topkg_care.Opam.File.field_names}.  {b Warning.} In [`Pin]
         {!Env.build}s, [`Opam] watermarks will only get substituted
         if the package [topkg-care] is installed.}}

      When a file is watermarked with an identifier ["ID"], any occurence of
      the sequence [%%ID%%] in its content is substituted by its definition. *)

  type distrib
  (** The type for describing distribution creation. *)

  val distrib :
    ?watermarks:watermark list ->
    ?files_to_watermark:(unit -> fpath list result) ->
    ?massage:(unit -> unit result) ->
    ?exclude_paths:(unit -> fpath list result) ->
    ?uri:string -> unit -> distrib
  (** [distrib ~watermarks ~files_to_watermark ~massage
      ~exclude_paths ~uri ()] influences the distribution creation
      process performed by the [topkg] tool.
      See the {{!distdetails}full details about distribution creation}.

      In the following the {e distribution build directory} is a
      private clone of the package's source repository's [HEAD] when
      [topkg distrib] is invoked.

      {ul
      {- [watermarks] defines the source watermarks for the distribution,
         defaults to {!watermarks}.}
      {- [files_to_watermark] is invoked in the distribution build
         directory to determine the files to watermark, defaults
         to {!files_to_watermark}.}
      {- [massage] is invoked in the distribution build directory,
         after watermarking, but before archiving. It can be used to
         generate distribution time build artefacts. Defaults to {!massage}.}
      {- [exclude_paths ()] is invoked in the distribution build
         directory, after massaging, to determine the paths that are
         excluded from being added to the distribution archive. Defaults to
         {!exclude_paths}.}
      {- [uri] is an URI pattern that specifies the location of the
         distribution on the WWW. In this string any sub-string
         ["$(NAME)"] is replaced by the package name, ["$(VERSION)"] is replaced
         by the distribution version string and ["$(VERSION_NUM)"] by the
         distribution version string, chopping an initial
         ['v'] or ['V'] character if present. This argument is used to
         generate the [url] file of an OPAM package for the distribution;
         it will be deprecated in the future in favour of a [x-distrib-uri]
         field in the OPAM file. If the value is unspecified it defaults to:
{[PKG_HOMEPAGE/releases/$(NAME)-$(VERSION_NUM).tbz]}
         where PKG_HOMEPAGE is the package's OPAM file [homepage] field.
         As a special case if the
         hostname of PKG_HOMEPAGE is [github] the following is used:
{[PKG_DEV_REPO/releases/download/$(VERSION)/$(NAME)-$(VERSION_NUM).tbz]}
         where PKG_DEV_REPO is the package's OPAM file [dev-repo] field
         without the [.git] suffix.}} *)

  val watermarks : watermark list
  (** [watermarks] is the default list of watermarks. It has the following
      elements:
      {ul
      {- [("NAME", `Name)]}
      {- [("VERSION", `Version)]}
      {- [("VCS_COMMIT_ID", `Vcs [`Commit_id])]}
      {- [("PKG_MAINTAINER", `Opam (None, "maintainer", ", "))]}
      {- [("PKG_AUTHORS", `Opam (None, "authors", ", ")]}
      {- [("PKG_HOMEPAGE", `Opam (None, "homepage", " ")]}
      {- [("PKG_ISSUES", `Opam (None, "bug-reports", " ")]}
      {- [("PKG_DOC", `Opam (None, "doc", " "))]}
      {- [("PKG_LICENSE", `Opam (None, "license", ", ")]}
      {- [("PKG_REPO", `Opam (None, "dev-repo", " "))]}}
      Prepending to the list will override default definitions. *)

  val files_to_watermark : unit -> fpath list result
  (** [files_to_watermark ()] is the default list of files to
      watermark.  It is invoked in the distribution build directory
      and gets the set of {{!Vcs.tracked_files}tracked files} of this
      directory from which it removes the files that end with [.flv],
      [.gif], [.ico], [.jpeg], [.jpg], [.mov], [.mp3], [.mp4], [.otf],
      [.pdf], [.png], [.ttf], [.woff]. *)

  val massage : unit -> unit result
  (** [massage] is the default distribution massaging function. It is
      invoked in the distribution build directory and does nothing. *)

  val exclude_paths : unit -> fpath list result
  (** [exclude_paths ()] is the default list of paths to exclude
      from the distribution archive. It is invoked in the distribution build
      directory and returns the following static set of files.
{[
fun () -> Ok [".git"; ".gitignore"; ".gitattributes"; ".hg"; ".hgignore";
              "build"; "Makefile"; "_build"]]} *)

  (** {1 Package description} *)

  type std_file
  (** The type for specifying a standard file. *)

  val std_file : ?install:bool -> fpath -> std_file
  (** [std_file ~install p] is a standard file [p] expressed relative
      to the source repository root directory. The file is
      automatically installed if [install] is [true] (default). *)

  type meta_file
  (** The type for specifying an OCamlfind META file. *)

  val meta_file : ?lint:bool -> ?install:bool -> fpath -> meta_file
  (** [meta_file ~lint ~install p] is a META file [p] expressed relative
      to the source repository root directory. The file is automatically
      installed in the {!lib} field if [install] is [true] (default).
      If [lint] is [true] (default), it is OCamlfind linted. *)

  type opam_file
  (** The type for specifying an opam file. *)

  val opam_file :
    ?lint:bool -> ?lint_deps_excluding:string list option -> ?install:bool ->
    fpath -> opam_file
  (** [opam_file ~lint ~lint_deps_excluding ~install p] is an OPAM file
      [p] expressd relative to the source repository root directory such that:
      {ul
      {- If [install] is [true] (default), it is automatically installed
         in the {!lib} field.}
      {- If [lint] is [true] (default), it is OPAM linted.}
      {- If [lint_deps_excluding] is [Some excludes], [topkg]
         checks that each of the OPAM package dependencies is mentioned
         as a root package in the OCamlbuild [_tags] file and vice-versa. The
         following package names are excluded from this test:
         {ul
         {- The packages names mentioned in [excludes].}
         {- Package names that start with ["conf-"]}
         {- {!Topkg_care.OCamlfind.base_packages}}
         {- {!Topkg_care.Opam.ocaml_base_packages}}}
         If [None] the dependency check is disabled.}} *)

  val describe :
    ?delegate:Cmd.t ->
    ?readme:std_file ->
    ?license:std_file ->
    ?change_log:std_file ->
    ?metas:meta_file list ->
    ?opams:opam_file list ->
    ?lint_files:fpath list option ->
    ?lint_custom:(unit -> R.msg result list) ->
    ?distrib:distrib ->
    ?build:build ->
    string -> (Conf.t -> install list result) -> unit
  (** [describe name install] describes a package named [name] with:
      {ul
      {- [delegate], the package delegate command to use. If unspecfied
         determined by the delegate lookup procedure, see
         [topkg help delegate] for more information.}
      {- [readme] is a readme file, defaults to {!std_file}
         [ "README.md"].  Automatic install is in the {!doc} field.}
      {- [license] is a license file, defaults to {!std_file}
         [ "LICENSE.md"].  Automatic install is in the {!doc} field.}
      {- [change_log] the change log, defaults to {!std_file}
         [ "CHANGES.md"].  Automatic install is in the {!doc} field.}
      {- [metas] the package's ocamlfind META files, defaults to
         {!meta_file} [ "pkg/META"].}
      {- [opams] the package's OPAM package files, defaults to
         {!opam_file} [ "opam"]. TODO describe how OPAM files
         are looked up according to the package name.}
      {- [lint_files] if [Some files], ensures that all files mentioned in
         [readme], [license], [change_log], [metas], [opams] and [files]
         are present in the distribution. Defaults to [Some []].
         If [None] disables the test.}
      {- [lint_custom] defines a custom linting process run with the current
         directory set at the root of the distribution. Successes and errors
         in the returned list are reported as such and any error in the list
         makes the lint fail. Defaults to [None].}
      {- [distrib], specifies the distribution process, defaults to
         {!distrib}[ ()].}
      {- [build], specifies the build process, defaults to {!build}[ ()].}
      {- [install] given a {{!Conf.t}build configuration} specifies
         the install moves. Note that some of standard files are
         automatically installed and don't need to be specified, see
         {!std_file}, {!meta_file} and {!opam_file}.}} *)

  (** {1:distdetails Package distribution creation details}

      The following describes the exact steps performed by [topkg
      distrib] to create the distribution archive. Note that [topkg]
      allows to override or disable part of the process via command
      line arguments, e.g. to specify the version string manually or
      skip linting. See [topkg distrib --help] for more information.

      The distribution process assumes that the source repository
      working directory is clean so that its definitions are consistent
      with those of the distribution build directory. A warning is
      generated if this is not the case as it may end up in inconsistent
      distribution archives (but which may be fine to only publish
      a documentation update).

      Let [$NAME] be the name of the package, [$BUILD] be its
      {{!build}build directory}, [$VERSION] be the VCS tag description
      (e.g.  [git-describe(1)] if you are using [git]) of the source
      repository HEAD commit and [distrib] the {{!distrib}distribution
      description} found in the source's repository [pkg/pkg.ml] file.
      {ol
      {- Clone the source repository at [HEAD] as the distribution build
         directory [$BUILD/$NAME-$VERSION.build].}
      {- Prepare the distribution:
        {ol
         {- Invoke the [files_to_watermark] function of [distrib] in the
            distribution build directory to determine the files to watermark
            with [watermarks] and perform the watermarking process.}
         {- Run the [massage] function of [distrib] in the distribution
            build directory. This can be used to create distribution time
            build artefacts.}}}
      {- Invoke the [exclude_paths] function of [distrib] in the
         distribution build directory to determine the paths to exclude
         from the archive.}
      {- Create a distribution tarball [$BUILD/$NAME-$VERSION.tbz] with the
         file hierarchy in [$BUILD/$NAME-$VERSION.build],
         excluding the paths determined at the preceeding point and delete the
         clone [$BUILD/$NAME-$VERSION.build]. File modifications times in
         the archive are set to [HEAD]'s commit time and file
         permissions are preserved. Any other form of file metadata is
         discarded in the archive.}
      {- Test the distribution. Unpack it in directory [$BUILD/$NAME-$VERSION],
         lint the distribution, build the package in the current
         build environment, on success delete [$BUILD/$NAME-$VERSION].
         Note that this uses the archive's [pkg/pkg.ml] file, which
         should not be different from the source's repository file
         if the latter was clean when [topkg distrib] was invoked.}}

      {2 Note on watermarking}

      It is right to doubt the beauty and be concerned about the
      watermarking process. However experience shows that alternatives
      like having an OCaml module generated with the appropriate
      information doesn't work well in practice. Version numbers do
      not only show up in OCaml source code. They also appear in
      documentation comments, metadata files, textual data files and
      non-OCaml source files.

      Watermarking by default all the non binary files of the
      distribution allows one to write %‌%VERSION%% in any context and
      be sure it will be substituted with the right version number in
      pin and distribution {{!Conf.build_context}build contexts}
      (this occurence was not subsituted
      because a ZERO WIDTH NON-JOINER U+200C was introduced between
      the first two percent characters).

      If this scheme poses a problem for certain files or you remain
      unconvinced, simply filter the result of {!files_to_watermark} or
      replace it by the exact files you would like to watermark.  *)
end

(** {1 Private} *)

(** Private definitions.

    {b Warning.} The following definitions are subject to change even
    between minor versions of the library. [Topkg] users {b must not}
    use these definitions to describe their package. *)
module Private : sig

  (** {1 Private} *)

  val disable_main : unit -> unit
  (** [disable_main ()] disables [Topkg]'s main invoked on
      {!Pkg.describe}. Invoke this function in your main function if
      you are not using [Topkg] in a description file but as as a
      library. *)

  (** Topkg interprocess communication codec.

      Codecs for communication between the [topkg] tool and topkg
      description files. *)
  module Codec : sig

    (** {1 Decode errors} *)

    (** The type for decode errors.
        {ul
        {- [Corrupted (kind, data)], an error occured while decoding
           [data] for [kind].}
        {- [Version (exp, fnd)], a {{!version}versioned} decoder
           expected version [exp] but found [fnd]}} *)
    type error = Corrupted of (string * string) | Version of int * int

    val pp_error : Format.formatter -> error -> unit
    (** [pp_error ppf e] prints an unspecified representation of [e]
        on [ppf]. *)

    exception Error of error
    (** Raised on decode errors. *)

    (** {1 Codecs} *)

    type 'a t
    (** The type for codec for OCaml values of type ['a]. *)

    val v : kind:string -> enc:('a -> string) -> dec:(string -> 'a) -> 'a t
    (** [v kind enc dec] is a codec for value identified as [kind] using
        [enc] to encode and [dec] to decode. *)

    val kind : 'a t -> string
    (** [kind c] is [c]'s kind. *)

    val enc : 'a t -> 'a -> string
    (** [enc c] is [c]'s encoder. *)

    val dec : 'a t -> string -> 'a
    (** [dec c] is [c]'s decoder. The decoder @raise Error in case of
        decode error *)

    val dec_result : 'a t -> string -> 'a result
    (** [dec c] is like {!dec} but doesn't raise. The exception is
        turned into an error message using {!pp_error}. *)

    val with_kind : string -> 'a t -> 'a t
    (** [with_kind k c] is [c] with kind [k]. *)

    val write : fpath -> 'a t -> 'a -> unit result
    (** [write f c v] encodes value [v] with [c] to [f]. *)

    val read : fpath -> 'a t -> 'a result
    (** [read f c] reads a value with [c] from [f]. *)

    (** {1 Base type codecs} *)

    val unit : unit t
    (** [unit] codecs a [()]. *)

    val bool : bool t
    (** [bool] codecs booleans. *)

    val int : int t
    (** [int] codecs integers. *)

    val string : string t
    (** [string] codecs strings. *)

    val option : 'a t -> 'a option t
    (** [option el] codecs [el] options. *)

    val result : ok:'a t -> error:'b t -> ('a, 'b) Result.result t
    (** [result ~ok ~error] codecs [ok], [error] results. *)

    val list : 'a t -> 'a list t
    (** [list el] codecs [el] lists. *)

    val pair : 'a t -> 'b t -> ('a * 'b) t
    (** [pair c0 c1] codecs [c0], [c1] pairs. *)

    val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
    (** [t3] is like {!pair} but for triples. *)

    val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
    (** [t4] is like {!pair} but for quadruples. *)

    val t5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t ->
      ('a * 'b * 'c * 'd * 'e) t
    (** [t5] is like {!pair} but for qintuples. *)

    val version : int -> 'a t -> 'a t
    (** [version num c] versions codec [c] with number [num].
        On decode a version number mismatch raises an error
        see {!error}. *)

    val view : ?kind:string -> ('a -> 'b) * ('b -> 'a) -> 'b t -> 'a t
    (** [view kind t c] views [t] as [c] for codecing. *)

    (** {1 Topkg types} *)

    val msg : [`Msg of string ] t
    (** [msg] codecs error messages. *)

    val result_error_msg : 'a t -> 'a result t
    (** [result_error_msg ok] codecs [ok] or error message results. *)

    val fpath : Topkg_fpath.t t
    (** [fpath] codecs files paths. *)

    val cmd : Topkg_cmd.t t
    (** [cmd] codecs command line fragments. *)
  end

  (** Package description. *)
  module Pkg : sig

    type t
    (** The type for package descriptions. *)

    val empty : t
    (** [empty] is an empty package description. *)

    val name : t -> string
    (** [name p] is [p]'s name. *)

    val delegate : t -> Cmd.t option
    (** [delegate p]is [p]'s delegate. *)

    val build_dir : t -> fpath
    (** [build_dir p] is [p]'s build directory. *)

    val readme : t -> fpath
    (** [readme p] is [p]'s readme file. *)

    val change_log : t -> fpath
    (** [change_log p] is [p]'s change log file. *)

    val license : t -> fpath
    (** [license p] is [p]'s license file. *)

    val opam : name:string -> t -> fpath
    (** [opam name p] is [p]'s OPAM file for OPAM package [name]. *)

    (** {1:distrib Distrib} *)

    val distrib_uri : t -> string option
    (** [distrib_uri p] is [p]'s distribution location URI pattern.
        See {!Pkg.distrib}. *)

    (** {1:lints Lints}

        {b Note.} In the following [None] values mean that
        the lint is disabled by the package description. *)

    val lint_custom : t -> (unit -> R.msg result list) option
    (** [lint_custom p] is [p]'s custom linting function (if any).

        {b Note.} Use {!Ipc.lint_custom} to run the function
        from another program. *)

    val lint_files : t -> fpath list option
    (** [lint_files p] are [p]'s files to check for existence. *)

    val lint_metas : t -> (fpath * bool) list
    (** [lint_metas p] are [p]'s META file to OCamlfind lint. *)

    val lint_opams : t -> (fpath * bool * string list option) list
    (** [lint_opams p] are [p]'s OPAM file OPAM lint and dependency
        lint. *)

    (** {1:codec Codec} *)

    val codec : t Codec.t
    (** [codec] is a codec for package descriptions. *)
  end

  (** Topkg interprocess communication. *)
  module Ipc : sig

    (** {1 Interprocess communication} *)

    type 'a t
    (** The type for interpocess communication transfering values of
        type ['a]. *)

    val cmd : 'a t -> Cmd.t
    (** [cmd ipc] are the command line arguments provided to the child
        process. *)

    val codec : 'a t -> 'a Codec.t
    (** [codec ipc] is the codec used to transfer the value. *)

    (** {1 Requests} *)

    val pkg : Pkg.t t
    (** [pkg] is an IPC to get the package description. *)

    val lint_custom : R.msg result list option t
    (** [lint_custom] is an IPC to run the custom linting. *)

    val distrib_prepare :
      dist_build_dir:fpath -> name:string -> version:string -> opam:fpath ->
      fpath list result t
    (** [distrib_prepare dist_build_dir name version opam] is an IPC to
        prepare a distribution in directory [dist_build_dir]. This
        sets the cwd to [dist_build_dir], performs the distribution
        watermarking process with [name] used for [`Name], [version] used
        for [`Version] and [opam] as the default file for OPAM watermarks.
        It then performs distribution massaging and returns the file paths
        to exclude from the distribution archive. *)
  end

  (** OPAM helpers. *)
  module Opam : sig

    (** {1 OPAM} *)

    (** OPAM package file access.

        Normally OPAM metadata access is only needed at distribution
        time and this is handled by {!Topkg_care.Opam.File} using the
        [opam-lib] library.

        However there is one case where we want to be able to access
        the metadata from [Topkg]: on pin builds where the
        {{!Pkg.watermark}watermarking} process needs to be run to turn
        the repo into a pseudo-distribution.

        Since we don't want [Topkg] to have any dependency and that
        [opam] currently doesn't allow to consult the fields of arbitrary
        OPAM files (see
        {{:https://github.com/ocaml/opam/issues/2446} issue #2446}) we
        assume a pin build will have the [topkg] tool installed and call
        to it to get the OPAM fields for watermarking (if [topkg] is
        unavailable the watermarks will simply be undefined). *)
    module File : sig

      (** {1 OPAM file} *)

      type t = (string * string list) list
      (** The type for a simplified model the fields of an OPAM
          file. See {!Topkg_care.Opam.File}. *)

      val codec : t Codec.t
      (** [codec] is a codec for OPAM file fields. *)

      val fields : fpath -> ((string * string list) list) result
      (** [fields file] are the fields of the OPAM file [file] which
          are obtained by calling the [topkg] topkg executable. *)
    end
  end
end

(** {1:basics Basics}

{!Topkg} is a packager for distributing OCaml software. At
its heart it provides a simple and flexible mechanism to describe
an OPAM {{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}[install]
file} according to the build configuration and make one corresponding
invocation in your build system.

This simple idea brings the following advantages:
{ol
{- It frees you from implementing an install procedure in your build
   system: this task is delegated to {{:http://opam.ocaml.org}OPAM},
   to the [opam-installer] tool or anything that understands an OPAM
   install file.}
{- It doesn't reclaim control over your build system. It will only
   invoke it {e once} with a list of targets determined by the package
   description according to a build configuration explicitely specified
   on the command line.}
{- It is very flexible and supports a wide range of installation scenarios.}}

Beyond this a [Topkg] package description provides information about
the package's distribution creation and publication procedures. This
enables swift and correct package distribution management via the [topkg]
tool, see {!care}.

{ul
{- {!setup}}
{- {!build}}
{- {!descr}}
{- {!old}}
{- {!care}}
{- Advanced topics
   {ul {- {!config_store}}
       {- {!multiopam}}}}
{- {!menagerie}}}

{1:setup Source repository setup}

The root of you source repository should have the following layout:
{ul
{- [pkg/pkg.ml], the package description written with {!Topkg}'s API.
   See {{!descr}package description.}}
{- [pkg/META], an
    {{:http://projects.camlcity.org/projects/findlib.html}ocamlfind}
    META file describing your package. This file is automatically installed
    in the lib directory. See {!Pkg.describe} to configure this.}
{- [_tags], ocamlbuild file with at least a [true : bin_annot] line.
   See {{!cmt}handling cmt and cmti} files for details.}
{- [opam], the package's OPAM metadata. See {!build}.}
{- [README.md], your readme file. See {!Pkg.describe} to configure this.}
{- [LICENSE.md], your license file. See {!Pkg.describe} to configure this.}
{- [CHANGES.md], your change log. See {!Pkg.describe} to configure this.}}

{2:carcass_ad Quick setup (advertisement)}

If you start a new library
{{:http://erratique.ch/software/carcass}[carcass]} can generate
the structural boilerplate with your personal information. Invoke:
{v
carcass body topkg/pkg mypkg
(cd mypkg && git init && git add . && git commit -m "First commit.")
opam pin add -kgit mypkg mypkg#master
v}

and you have a library that is [{opam,ocamlfind}]-able with correct
version watermarks on releases and opam pins. You are now only a few
invocations away to release it in the OCaml OPAM repository, see
[topkg help release] for doing so; but don't forget to document it and
make it provide something useful.

{1:build OPAM and package build instructions}

The package needs to build-depend on [topkg] aswell as [ocamlfind]
which is used by the package description file [pkg/pkg.ml] to find the
[topkg] library; it is likely that you are using [ocamlbuild] too. So
the depends field of your OPAM file should at least have:

{v
depends: [
  "ocamlfind" {build}
  "ocamlbuild" {build}
  "topkg" {build} ]
v}

The build instructions of the package are simply an invocation of
[pkg/pkg.ml] with the [build] command and a specification of the
build configuration on the command line. In the simplest case, if
your package has no configuration options, this simply boils
down to:
{v
build: [[
  "ocaml" "pkg/pkg.ml" "build"
          "--installer" "true" ]]
v}

The ["--installer" "true"] configuration key specification is used to
inform the package description about the {{!Conf.build_context}build
context}. This invocation of [pkg/pkg.ml] executes your build system
with a set of targets determined from the build configuration and
generates in the root directory of your distribution an OPAM [install]
file that OPAM uses to install and uninstall your package.

This is all you need to specify. Do not put anything in the remove
field of the OPAM file. Likewise there is no need to invoke [ocamlfind] with
your [META] file. Your [META] file should simply be installed in the
[lib] directory which happens automatically by default.

{b Beyond OPAM.} If you need to support another package system you
can invoke [pkg/pkg.ml] as above and then manage the installation and
uninstallation at a given [$DESTDIR] with the generated OPAM [install]
file using [opam-installer] tool or any other program that understand
these files.

{1:descr Package description}

The {!Pkg.describe} function has a daunting number of arguments and
configuration options. However if you keep things simple and stick to
the defaults not much of this will need to be specified. In fact if we
consider the basic default {{!setup}setup} mentioned above for a library
described to OCamlbuild in a [src/mylib.mllib] file your package description
file [pkg/pkg.ml] will simply look like this:
{[
#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mylib" @@ fun c ->
  Ok [ Pkg.mllib "src/mylib.mllib" ]
]}

{b Tip.} To allow
{{:https://github.com/the-lambda-church/merlin}merlin} to function
correctly in your package description, issue [M-x merlin-use topkg] in
[emacs] or [:MerlinUse topkg] in [vim].

Try to test the package build description with [topkg build], this
will build the package and write a [mylib.install] that describes the
package installation. You are now ready to release, see [topkg help
release] for the procedure.

To debug package descriptions it useful to dry run the build. This
prevents the package from building and only writes the [mylib.install] file
determined according to the build configuration.
{[
topkg build -d    # Only write the OPAM install file
topkg build -d -v # Also print the build configuration
]}
Note that [topkg build] does nothing more than invoke
[ocaml "pkg/pkg.ml" build].  If you would like to see the build
{{!section:Conf.key}configuration
options} of a package description you should do:
{v
ocaml pkg/pkg.ml help
./pkg/pkg.ml help     # If has exec right
v}

{1:old FIXME old material whose gist should be rewritten}

An OPAM [install] file is a description of a standard UNIX
install. It has fields for each of the standard directories [lib],
[bin], [man], [etc], etc. Each of these fields lists the files to
install in the corresponding directory (or subdirectories). See the
{{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}install
file specification} in the OPAM developer manual for more information.

In its simplest form, the package description file [pkg/pkg.ml] is simply
In {!Topkg}, the package description file  is simply:
{ol
{- A {{!section:Pkg.builder}builder specification}. It specifies the
   build tool that is invoked once with the files to build and install
   as determined by the current build configuration.}
{- A specification of the files to install according to the build
   configuration by specifying invoking install field functions.

one wants build to put in each field
   of the package's [.install] file by invoking install field
   functions [Pkg.{lib,bin,doc,...}]}}

Here is an example of a [pkg/build.ml] description for a single module
library that also installs a command line tool called [jsontrip]. Don't
worry about the sheer length of the description we'll soon cut down
on the number of lines.
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

{b Tip.} If you are using
{{:https://github.com/the-lambda-church/merlin}[merlin]} and [emacs],
issue a [M-x merlin-use topkg], to get merlin support for editing
the package description.

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

{2:exts Extension sets}

The install field functions have an optional [exts] argument. If
present these extensions are appended to the path given to the
function. The module [[Exts]](pkg/topkg.ml#L23) defines a few
predefined extension sets. For example a module library implemented in
[src/mylib.ml] can be declared by:
{[
Pkg.lib ~exts:Exts.module_library "src/mylib"
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

{2:autobin Auto binaries}

For generating an installing native binaries if native code
compilation is available and byte code binaries if not you can use the
[auto] optional argument of [Pkg.bin] and [Pkg.sbin]. Using it with
[true] you can simply specify the binary name prefix. It will use the
base name as the name of the tool and ask for either a [.native] or
[.byte] target depending if native compilation is available or not.

{[
Pkg.bin ~auto:true "src/mybinary"
]}

{2:conds Conditions}

Conditional installation is handled through the optional argument
[cond] of install field functions. If [cond] is [false] it's neither
built nor installed. For example for a library that depends on the
presence of another:

{[
let otherlib = Env.bool "otherlib"
...
Pkg.lib ~cond:otherlib ~exts:Exts.module_library "src/mylib"
]}

Conditions related to native code and native code dynamic linking
availability happen automatically:

* In [Pkg.lib] paths ending with [.cmxs] are dropped if
  [Env.native_dynlink] is [false] and paths ending with
  [.a], [.cmx], [.cmxa] and [.cmxs] are dropped if
  [Env.native] is [false].

* In [Pkg.{bin,sbin}] path ending with [.native] are dropped
  if [Env.native] is false.

{2:env Environment}

New boolean keys are added to the environment by calling [Env.bool
key]. To output a sample environment you can invoke the build script with
[--help]:

    ocaml pkg/pkg.ml --help

{2:rename Renaming and installing in subdirectories}

By default install field functions use the basename of the path given
to the function as the install name. If you need to rename the build
artefact or install to a subdirectory you can use the [dst]
optional argument of install field functions. For example for a
library that needs to be installed in a [subdir] subdirectory of
[lib] use:

{[
Pkg.lib ~exts:Exts.module_library ~dst:"subdir/mylib" "src/mylib"
]}

{2:cmt Handling cmt and cmti files}

Since the OCaml tools generate [.cmt] and [.cmti] files only as a side
effect they are treated specially: they are not built. For
[ocamlbuild] you should add this line to your [_tags] file:
{[
true : bin_annot
]}
this will build them as a side effect of other build invocations. In
the [$PKG.install] file generated by topkg the [cmt] and [cmti] files
are prefixed by a ? so that if they are not built (pre OCaml 4.01 for
[ocamlbuild]) the install doesn't fail.

{2:map Map from descriptions to targets and build artefacts}

{!Topkg} has been designed with [ocamlbuild] in mind but it should be
usable with any other build system as long as it is able to understand
the targets topkg {{!map}generates}.


Given an invocation [Pkg.{lib,bin,...} "$PATH"], the system generates
a target [$PATH] for your build system and expects to find the build
artefact in [$BUILD/$PATH] where [$BUILD] is the build directory of
the build tool (e.g. [_build] for [ocamlbuild]).

{1:care Package care}

Developing and distributing packages implies a lot of mundane but
nevertheless important tasks. The [topkg] tool, guided by information
provided by {!Pkg.describe} helps you with these tasks.

For example [topkg lint] makes sure that the package source repository
or distribution follows a few established (or your) conventions and
that the META and OPAM files of the package pass their respective
linter.  [topkg distrib] will create watermarked and reproducible
distribution archives (see {!Pkg.distrib}) while [topkg publish] and
[topkg opam] will help publishing them. All this and more is described
with details in [topkg]'s help system, invoke:
{v
topkg help release
topkg help
 v}

to get an extended introduction and pointers to these features.

{2:doccare Documentation care}

Topkg provides support to make it easier to write and publish your
package documentation. The [topkg doc -r] command generates and refreshes
renderings of the package documentation while you work on it.

Documentation publication is always derived from a generated
distribution archive (the latter doesn't need to be published of
course). So to push documentation fixes and clarifications simply invoke:

{v
topkg distrib
topkg publish doc
v}

Make sure you include %‌%VERSION%% watermarks in the documentation so
that readers know exactly which version they are reading. By default
this will be substituted by a VCS tag description so it will be
precise even though you may not have properly tagged a new release
for the package.

{1:advanced Advanced topics}

{2:config_store Storing build configuration information in software}

The following sample setup shows how to store build configuration
information in build artefacts.

In this example we store the location of the install's [etc] directory
in the build artefacts. The setup also works seamlessly during
development if build artefacts are invoked from the root directory of
the source repository.

We have the following file layout in our source repository:
{v
etc/mypkg.conf       # Configuration file
src/mypkg_etc.ml     # Module with path to the etc dir
v}

the contents of [src/mypkg_etc.ml] is simply:
{[
(* This file is overwritten by distribution builds. During development
   it refers to the [etc] directory at the root directory of the
   source repository. *)

let dir = "etc"
]}
the value [Mypkg_etc.dir] is used in the sources to refer to the [etc]
directory of the install. In the package description file
[pkg/pkg.ml] we have the following lines:
{[
let (* 1 *) etc_dir =
  let doc = "Use $(docv) as the etc install directory" in
  Conf.(key "etc-dir" fpath ~absent:"etc" ~doc)

let (* 2 *) etc_config c = match Conf.build_context c with
| `Dev -> Ok () (* Do nothing, the repo src/mypkg_etc.ml will do *)
| `Pin | `Distrib ->
    let config = strf "let dir = %S" (Conf.value c etc_dir) in
    OS.File.write "src/mypkg_etc.ml" config

let () =
  let build = Pkg.build ~pre:etc_config () in
  Pkg.describe "mypkg" ~build @@ fun c ->
  Ok [ (* 3 *) Pkg.etc "etc/mpypkg.conf"; ... ]
]}

In words:
{ol
{- We declare a configuration key ["etc-dir"] that holds the location
of the install [etc] directory.}
{- We have a pre-build hook that writes the file
[src/mypkg_etc.ml] with its actual value on [`Pin] and [`Distrib] builds.}
{- We install the [etc/mypkg.conf] configuration in the install [etc]
   directory.}}
The OPAM build instructions for the package are:
{v
build:
[[ "ocaml" "pkg/pkg.ml" "build"
           "--installer" "true"
           "--etc-dir" "%{mypkg:etc}%" ]]
v}

{2:multiopam Multiple OPAM packages for a single distribution}

It is not too hard to define multiple OPAM packages for the same
distribution. Topkg itself
{{:https://github.com/dbuenzli/topkg/blob/master/DEVEL.md}uses this
trick} to manage its dependencies between [topkg] and [topkg-care].

To achieve this your package description file can simply condition
the package install description on the package name
{{!Conf.pkg_name}communicated by the configuration}. In this setup
you'll likely have one [$PKG.opam] per [$PKG] at the root of your source
repository, you should declare them to the description too, so that
they get properly linted and used by the [topkg] tool when appropriate
(TODO describe how the OPAM file is looked up according to the package name
in {!Pkg.describe}). Here is a blueprint:
{[
let () =
  let opams =
    let install = false in
    [ Pkg.opam_file ~install "mypkg-main.opam";
      Pkg.opam_file ~install "mypkg-snd.opam"; ]
  in
  Pkg.describe ~opams "mypkg-main" @@ fun c ->
  match Conf.pkg_name c with
  | "mypkg-main" ->
      Ok [ Pkg.lib "mypkg-main.opam" ~dst:"opam";
           (* mypkg-main install *) ]
  | "mypkg-snd" ->
      Ok [ Pkg.lib "mypkg-snd.opam" ~dst:"opam";
           (* mypkg-snd install *) ]
  | other ->
      R.error_msgf "unknown package name: %s" other
]}
The build instructions of these OPAM files need to give the name of
the package to the build invocation so that the right install description
can be selected:
{v
build:
[[ "ocaml" "pkg/pkg.ml" "build"
           "--pkg-name" "%{name}%"
           "--installer" "true" ]]
v}

In general you will use the default, main, package name and its OPAM file to
create and publish the distribution archive file and all packages
will use the same distribution; the OPAM packages will only differ in their
OPAM file. Releasing the set of packages then becomes:
{v
# Release the distribution and base package (use topkg bistro
# for doing this via a single invocation)
topkg distrib
topkg publish
topkg opam pkg
topkg opam submit

# Create and release the other OPAM package based on the ditrib
topkg opam pkg --pkg-name mypkg-snd
topkg opam submit --pkg-name mypkg-snd
v}

See [topkg help release] for more information about releasing
packages with [topkg].

{1:menagerie Menagerie of [pkg.ml] files}

With a description of what they showcase.

{{:https://github.com/dbuenzli/hmap/blob/master/pkg/pkg.ml}Hmap}
{ul {- Single module library. The simplest you can get.}}

{{:https://github.com/dbuenzli/fpath/blob/master/pkg/pkg.ml}Fpath}
{ul {- Single module library with toplevel support.}}

{{:https://github.com/dbuenzli/uucp/blob/master/pkg/pkg.ml}Uucp}
{ul
{- One library namespaced by a single module.}
{- Generation of distribution time release artefacts.}
{- Pin specific build preparation (download the UCD XML file).}}

{{:https://github.com/dbuenzli/mtime/blob/master/pkg/pkg.ml}Mtime}
{ul
{- C stubs}
{- Optional dependencies ([js_of_ocaml])}}

{{:https://github.com/dbuenzli/carcass/blob/master/pkg/pkg.ml}Carcass}
{ul
{- One library archive namespaced by a single module.}
{- One single module library archive.}
{- One executable.}
{- {{!config_store}Stores} install [etc] location in the software artefacts,
   using a {{!Pkg.build}pre-build hook}.}
{- Adjusts the watermarking process to ignore the files in the [etc] file
   hierarchy.}
{- Install files from the source tree, i.e. files that are not built, those
   in the [etc] hierarchy of the distribution.}}

{{:https://github.com/dbuenzli/topkg/blob/master/pkg/pkg.ml}Topkg}
{ul
{- Ignore the funky source bootstraping ([#mod_use] directives), that's
   only for using [topkg] on itself.}
{- Shows how to make {{!multiopam}multiple OPAM packages} for the same
   distribution.}
{- Manual [META] install, a single one is installed for all OPAM packages
   by the base package. This leverages the [if_exists] ocamlfind mecanism.}
{- Multiple OPAM file declaration and dependency linting exclusions:
   the build system mentions packages that are not relevant to
   all OPAM files.}}
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