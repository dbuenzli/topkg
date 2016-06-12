

- Change pin build detection (#44). This changes the way package
  should be build in in OPAM files. Substitute `"--installer" "true"`
  with `"--pinned" "%{pinned}%"` in your package descriptions. The
  `--installer` option is deprecated and has no longer any effect.

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
