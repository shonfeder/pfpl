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
 (depends ocaml dune (um-abt (>= 0.1.5)))
 (tags
  (plt experimental)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
