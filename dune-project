(lang dune 2.8)
(generate_opam_files true)

(name pfpl)

(source
 (github shonfeder/pfpl))

(authors "Shon Feder <shon.feder@gmail.com>")
(maintainers "Shon Feder <shon.feder@gmail.com>")
(license MIT)
(homepage TODO)
(documentation TODO)
(bug_reports TODO)

(package
 (name pfpl)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
  (um-abt (and (>= 0.1.7) (<= 0.2)))
  (sexplib (and (>= v0.14.0) (<= v0.15.0))))
 (tags
  (plt experimental)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
