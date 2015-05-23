OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,thread \
		-libs unix,graphics
TARGET=native

all:mapReduce example pictureInPicture

mapReduce:
	$(OCAMLBUILD) $@.$(TARGET)

example:
	$(OCAMLBUILD) $@.$(TARGET)

pictureInPicture:
	$(OCAMLBUILD) $@.$(TARGET)


clean:
	$(OCAMLBUILD) -clean

realclean: clean
	rm -f *~

cleanall: realclean
