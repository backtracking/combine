(lang dune 2.4)

(name combine)

(license LGPL)

(authors
  "Remy El Sibaie <remy.el-sibaie@lip6.fr>, Jean-Christophe Filliâtre <jean-christophe.filliatre@lri.fr>")

(maintainers
  "Jean-Christophe Filliâtre <jean-christophe.filliatre@lri.fr>")

(source
 (uri git://github.com/backtracking/combine.git))

(bug_reports https://github.com/backtracking/combine/issues)

(homepage https://www.lri.fr/~filliatr/combine/)

(documentation https://www.lri.fr/~filliatr/combine/)

(generate_opam_files true)

(package
 (name combine)
 (synopsis "Combine is an OCaml library for combinatorics problem solving")
 (description
   "Combine is an OCaml library for combinatorics problem solving.\nThe Combine library contains four main modules:\n- Dlx: implements Knuth's dancing links\n- Zdd: implements Zero-suppressed binary decision diagrams\n- Emc: a common interface to modules Dlx and Zdd to solve the Exact Matrix Cover problem\n- Tiling: converts a 2D tiling problem into an EMC problem")
 (depends
  (ocaml
   (>= 4.07))
  (dune
   (>= 2.4))
  (menhir
   (>= 20200211))
  (num :with-test)
  (str :dev)
  (graphics :dev)
  (ocamlgraph :dev)))

(using menhir 2.1)
