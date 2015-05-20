OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix
TARGET=native

all:mapReduce example

mapReduce:
	$(OCAMLBUILD) mapReduce.$(TARGET)

example:
	$(OCAMLBUILD) example.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
