(lang dune 3.0)
(name bindoj-template)

(license Apache-2.0)
(maintainers
  "Hirotetsu Hongo <hg@kxc.inc>"
  "Haochen Xie <hx@kxc.inc>"
  "Shiya Yamaguchi <wd@kxc.inc>"
  "Yuta Sato <cr@kxc.inc>"
  "KXC Members <info@kxc.inc>")
(authors
  "Hirotetsu Hongo"
  "Haochen Xie"
  "Shiya Yamaguchi"
  "Yuta Sato"
  "KXC Members and Contributors")

(source
 (github kxcdev/bindoj))

(generate_opam_files true)

(package
 (name bindoj-template)
 (synopsis "A generative datatype toolkit for OCaml centric workflows")
 (depends
  (ppxlib (>= 0.26))
  (ocaml (>= 4.12))))
