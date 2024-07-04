
### Simple one on one chat.

Application could starts in two modes:
```
Run as server: chat [-i retry_in] [-p port]
Run as client: chat [-i retry_in] [-p port] host
Default port is 6666
```

### Quick start
```
opam install dune core_unix
git clone https://github.com/kubrack/ocaml_simple_chat.git
cd ocaml_simple_chat
```
then
```
dune exec -- chat # as server
dune exec -- chat localhost # as client 
```
or even
```
dune exec -- chat > new_sock.ml
dune exec -- chat localhost < lib/sock.ml
$ md5sum new_sock.ml lib/sock.ml
e55212ef864710ec6a0276c03452701a  new_sock.ml
e55212ef864710ec6a0276c03452701a  lib/sock.ml
```
