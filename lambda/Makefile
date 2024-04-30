
all: lambda parser lexer pp main
	ocamlc -o top lambda.cmo pp.cmo parser.cmo lexer.cmo main.cmo

lambda: lambda.ml lambda.mli
	ocamlc -c lambda.mli lambda.ml

parser: parser.mly
	ocamlyacc parser.mly
	ocamlc -c parser.mli parser.ml

lexer: lexer.mll
	ocamllex lexer.mll
	ocamlc -c lexer.ml

pp: pp.ml pp.mli
	ocamlc -c pp.mli pp.ml 

main: main.ml
	ocamlc -c main.ml

clean:
	rm -f lexer.ml parser.mli parser.ml *.cmi *.cmo *~

