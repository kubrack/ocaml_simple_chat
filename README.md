
### Simple one on one chat.

Application could starts in two modes:
```
Run as server: chat [-i retry_in] [-p port]
Run as client: chat [-i retry_in] [-p port] host
Default port is 6666
```

### Quick start
```
opam install dune core core_unix
git clone https://github.com/kubrack/ocaml_simple_chat.git
cd ocaml_simple_chat
dune exec -- chat # as server
dune exec -- chat localhost # as client 

```
