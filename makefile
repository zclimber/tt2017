.PHONY : hw1 clean

hw1 : hw1.out
	./hw1.out

hw1.out : hw1.mli hw1.ml hw1t.ml
	ocamlopt -o hw1.out hw1.mli hw1.ml hw1t.ml

clean :
	rm *.[^m]*
