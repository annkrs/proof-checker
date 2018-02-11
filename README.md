# proof-checker

Find details in "Treść".

### Requirements
OCaml

[installation](https://ocaml.org/docs/install.html)

ocamlfind
```sh
opam install ocamlfind
```

menhir
```sh
opam install menhir
```

Core

[installation](https://kwangyulseo.com/2014/03/04/installing-ocamlopamutopcore-library-on-ubuntu-saucy/)

Put the following in your `.ocamlinit` file to use `Core`:
```
#use "topfind"
#thread
#require "core"
#require "core_bench"

let () =
  try Topdirs.dir_directory (Sys.getenv "OCAML_TOPLEVEL_PATH")
  with Not_found -> ()
;;
```

### Usage

Type following commands before testing program
```sh
opam switch 4.05.0 
eval `opam config env`
```

To compile and run program with default tests just type
```sh
$ make
```

Assign file name to TEST variable to use other tests 
```sh
$ make TEST=imp
```

Available tests: neg, con, dis, bic, imp and axiom by default.

### Screen

![N|Solid](http://oi65.tinypic.com/2h5r0q8.jpg)
