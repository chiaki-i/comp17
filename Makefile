SOURCES = operator.ml type.ml syntax.ml parser.mly lexer.mll \
	  gensym.ml knormal.ml env.ml \
	  alpha.ml beta.ml eta.ml assoc.ml elim.ml constf.ml \
	  first.ml \
	  register.ml prealloc.ml alloc.ml \
	  code.ml \
	  main.ml
RESULT = compiler
OCAMLMAKEFILE = /home/isstaff/asai/include/OCamlMakefile
include $(OCAMLMAKEFILE)

.SUFFIXES: .x .run

%.s : %.ml headerIntel.s compiler
	cp headerIntel.s $@
	cat $< | ./compiler >> $@

%.x : %.s mainIntel.c
	gcc -m64 mainIntel.c $< -o $@

.x.run :
	time env PATH=".:${PATH}" $<
