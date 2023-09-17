(lang dune 3.10)

(name calc_ans)

(generate_opam_files true)

(source
 (github seanbreckenridge/calc_ans))

(authors "Sean Breckenridge")

(maintainers "Sean Breckenridge")

(license MIT)

(documentation https://github.com/seanbreckenridge/calc_ans)

(package
 (name calc_ans)
 (synopsis "A simple REPL calculator")
 (description "A simple REPL calculator, supports +, -, *, /, ^, %, parens, abs, floor, ceil, round and sqrt, with a persistent memory of the last answer")
 (depends ocaml dune)
 (tags
  (topics cli math calculator)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
