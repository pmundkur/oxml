
all: oxml

oxml: main.ml xmlparser.ml xmlparser.cmi
	ocamlc -g -o oxml xmlparser.ml main.ml

xmlparser.cmi: xmlparser.mli
	ocamlc xmlparser.mli

clean:
	-rm oxml main.cmi main.cmo xmlparser.cmi xmlparser.cmo
	-rm oxml.opt main.cmx main.o xmlparser.cmx xmlparser.o

opt: main.ml xmlparser.ml xmlparser.cmi
	ocamlopt -o oxml.opt xmlparser.ml main.ml
