
- Remove deprecated `--installer` configuration key.
- `Pkg.files_to_watermark`, make sure only files are returned (#58).
- Improve error message of some `Topkg.OS` functions (#57).

v0.7.5 2016-06-22 Cambridge (UK)
--------------------------------

- `topkg doc` add short option `-d` for `--dev`.
- Fix `Pkg.mllib`, module list was lowercased rather than uncapitalized.

v0.7.4 2016-06-17 Cambridge (UK)
--------------------------------

- Add test description and run support. New `topkg test` command.
- Add distribution publication description support. Allows to define the
  set of default publication artefacts in the package description. The cli
  syntax of `topkg publish` for alternative artefacts changes from
  `alt KIND` to `alt-KIND`.
- Distributed (and thus installed) OPAM files are now properly
  versioned via the `version:` field.
- Improve tarball reproducibility across systems by not relying on the
  VCS checkout state for determining the read and write rights (#43).
- OPAM package submission: use the `opam-publish` submit message
  to append the release notes to the submission.
- Toy GitHub delegate: improve user authentication by trying to steal
  an existing opam-publish token.
- Toy GitHub delegate: improve package documentation publication. Thanks
  to Thomas Gazagnaire for the patches.
- Error message and IPC logging level propagation improvements. Thanks to
  Thomas Gazagnaire for the help.

v0.7.3 2016-06-12 Cambridge (UK)
--------------------------------

- Change pin build detection (#44). This changes OPAM build
  instruction for packages. Substitute `"--installer" "true"` by
  `"--pinned" "%{pinned}%"` in build instructions. The
  `--installer` option is deprecated and has no effect.

v0.7.2 2016-06-02 Cambridge (UK)
--------------------------------

- `Pkg.describe`, allow multiple readme, change log and license files.
  The optional arguments `readme`, `change_log` and `license` become
  `readmes`, `change_logs`, `licenses` with the same default. When
  topkg needs to act on a change log (e.g. `topkg log`) or readme
  (e.g. `topkg opam descr`), it acts on the first element of
  `change_log` and/or `readmes`.

- Fix `Conf.vcs` discovery to only look for a git
  directory in the build directory (#42).

v0.7.1 2016-05-26 Arbaz (VS)
----------------------------

- Improve Windows support. Thanks to Andreas Hauptmann for the help.

v0.7.0 2016-05-22 La Forclaz (VS)
---------------------------------

First release. 
