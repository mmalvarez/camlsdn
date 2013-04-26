.PHONY: all doc
OCAMLBUILD = ocamlbuild -lib unix -use-ocamlfind 

all: 
	$(OCAMLBUILD) controller.byte
	$(OCAMLBUILD) client.byte

doc:
	$(OCAMLBUILD) doc.docdir/index.html

test: _build/Test.d.byte
	$<

clean:
	rm -rf _build
	rm -rf extracted
	rm -f client.byte
	rm -f controller.byte 
