(lang dune 1.10)
(generate_opam_files true)

(name idds)
(source (github netkat-lang/idds))
(license MIT)
(authors "Steffen Smolka <smolka@cs.cornell.edu>")
(maintainers "Steffen Smolka <smolka@cs.cornell.edu>")

(package
  (name idds)
  (synopsis "Identity-suppressed decision diagrams (IDDs)")
  (depends
    (ocaml (>= 4.04.0))
    (dune (and :build (>= 1.10)))
    (base (>= v0.12.0))
    (open (> 0))
    (ppx_deriving (>= 4.3))
    (ppx_inline_test (and :with-test (>= v0.12.0)))
    (ppx_jane (>= v0.12.0))
  )
)
