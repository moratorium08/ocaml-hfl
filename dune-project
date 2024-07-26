(lang dune 3.7)
(using menhir 2.0)
(name hfl)
(generate_opam_files)
(version 0.0.1)
(source (github hopv/ocaml-hfl))
(license Apache-2.0)
(authors "Naoki Iwayama" "Ken Sakayori")
(maintainers "Ken Sakayori <sakayori@kb.is.s.u-tokyo.ac.jp>")

(package
  (name hfl)
  (synopsis "OCaml library for manipulaing HFL formulas")
  ; (description "A longer description")
  (depends
    base
    fmt
    logs
    menhir
    menhirLib
    ppx_compare
    ppx_deriving
    ppx_let
   (ppx_sexp_conv (>= v0.16))
 ))
