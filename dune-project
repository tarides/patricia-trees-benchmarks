(lang dune 3.20)

(name patricia-tree-benchmarks)

(version dev)

(generate_opam_files)

(authors
 "Nicolas Osborne <nicolas.osborne@tarides.com"
 "Jules Aguillon <jules@j3s.fr>")

(source
 (github tarides/patricia-tree-benchmarks))

(license MIT)

(package
 (name patricia-tree-benchmarks)
 (depends
  bechamel
  notty
  bechamel-notty
  csv
  qcheck
  ptmap
  dmap
  gmap
  hmap
  containers-data
  containers
  zarith
  fmt
  qcheck-core
  calcium
  re
  trace
  trace-tef
  ppx_deriving
  ppx_hash
  ppx_inline_test
  patricia-tree))
