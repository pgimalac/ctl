EXEC = reader.out

CC = ocamlc
LEX = ocamllex
YACC = ocamlyacc

all:
	$(CC) -c formule.mli formule.ml
	$(CC) -c marqueur.mli marqueur.ml
	$(LEX) lexer.mll
	$(YACC) parser.mly
	$(CC) -c parser.mli parser.ml
	$(CC) -c lexer.ml
	$(CC) -c reader.ml
	$(CC) -o $(EXEC) formule.cmo lexer.cmo parser.cmo reader.cmo

clean:
	rm *.cmo *.cmi parser.ml parser.mli lexer.ml $(EXEC)
