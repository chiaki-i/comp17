SOURCES = operator.ml type.ml syntax.ml parser.mly lexer.mll \
	  gensym.ml knormal.ml env.ml \
	  first.ml \
	  register.ml prealloc.ml \
	  main.ml
RESULT = prealloc
OCAMLMAKEFILE = ~/include/OCamlMakefile
include $(OCAMLMAKEFILE)
