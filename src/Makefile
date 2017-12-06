OCAMLBUILD=ocamlbuild
OCAMLBUILD_OPTS=-use-menhir -use-ocamlfind -pkgs "ppx_sexp_conv"
SERVER_OPTS= -tag thread -pkgs "core,async,cohttp.async"
TEST_DIR=tests
TEST_SCRIPT=run.sh
DOC_DIR=aalc.docdir

TARGET=aalc
SERVER=aalserver

.PHONY: all test clean

all:
	export OCAMLFIND_IGNORE_DUPS_IN=/home/alex/.opam/4.02.3/lib/ocaml/compiler-libs
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(TARGET).native

server:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(SERVER_OPTS) $(SERVER).native

js:
	# $(OCAMLBUILD) $(OCAMLBUILD_OPTS) -pp \
	# "camlp4o -I /opt/local/lib/ocaml/site-lib js_of_ocaml/pa_js.cmo" \
	# -cflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml -libs js_of_ocaml \
	# -lflags -I,+js_of_ocaml,-I,+site-lib/js_of_ocaml aaljs.byte
	ocamlbuild -use-ocamlfind -pkgs "js_of_ocaml,js_of_ocaml.syntax,sexplib" -use-menhir -syntax camlp4o aaljs.byte
	js_of_ocaml +nat.js +weak.js aaljs.byte

doc:
	$(OCAMLBUILD) $(OCAMLBUILD_OPTS) $(DOC_DIR)/index.html

test: all
	cd $(TEST_DIR) && ./$(TEST_SCRIPT)

clean:
	rm -f $(SERVER).native $(TARGET).native *~ \#*
	$(OCAMLBUILD) -clean
	cd tests && ./clean.sh
