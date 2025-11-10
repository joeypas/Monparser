(lang dune 3.20)

(name monparser)

(generate_opam_files true)

(source
 (github joeypas/Monparser))

(authors "Joe Liotta <jliotta03@gmail.com>")

(license MIT)

(package
 (name monparser)
 (synopsis "Monadic Parser Combinator Library")
 (description "A longer description")
 (depends ocaml core)
 (tags
  ("monads" "parser combinator")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
