(lang dune 2.9)
(name metadb)

(generate_opam_files true)
(source (github nguermond/metadb))
(license GPL-3.0-or-later)
(authors "Nathan Guermond")
(maintainers "nguermond@proton.me")

(package
 (name metadb)
 (version 1.0.3)
 (synopsis "A database for storing and managing file metadata in JSON format")
 (description
   "Metadb is a library to locally store and manipulate a JSON database of file metadata. A key feature of Metadb is that a user may rename or move files within or between libraries, and Metadb will resolve missing or renamed files according to their MD5 hash. Metadb can also find duplicate files across libraries."
 )
 (documentation "https://nguermond.github.io/metadb/docs/metadb")
 (depends
  dune
  (fileutils (>= 0.5.3))
  (yojson (>= 2.0.0))
  (ocaml (>= 4.13.0))
))
