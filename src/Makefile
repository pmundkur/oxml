
all: oxml

oxml: main.ml xmlparser.ml xmlparser.cmi
	ocamlc -g -o oxml xmlparser.ml main.ml

xmlparser.cmi: xmlparser.mli
	ocamlc xmlparser.mli

clean:
	rm main.cmo xmlparser.cmi xmlparser.cmo

