SOURCES = operator.ml type.ml syntax.ml parser.mly lexer.mll \
	  gensym.ml knormal.ml env.ml \
	  first.ml \
	  register.ml prealloc.ml alloc.ml \
	  main.ml
RESULT = prealloc2
OCAMLMAKEFILE = /home/isstaff/asai/include/OCamlMakefile
include $(OCAMLMAKEFILE)
