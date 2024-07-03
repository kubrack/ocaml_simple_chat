(lang dune 3.16)

(name chat)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "kubrack@gmail.com")

(maintainers "kubrack@gmail.com")

(license BSD)

(documentation https://github.com/kubrack/ocaml_simple_chat/README.md)

(package
 (name chat)
 (synopsis "Simple one on one chat")
 (description "See README.md")
 (depends ocaml dune)
 (tags
  (topics "ocaml" "tcp" "chat" "netcat")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
