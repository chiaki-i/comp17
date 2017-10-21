SOURCES = operator.ml type.ml syntax.ml parser.mly lexer.mll \
	  gensym.ml knormal.ml \
	  main.ml
RESULT = knormal
OCAMLMAKEFILE = ~/include/OCamlMakefile
include $(OCAMLMAKEFILE)
