.PHONY : hw1 reduction clean

hw1 : hw1.out
	./hw1.out

hw1.out : hw1.mli hw1.ml hw1_test.ml
	ocamlc -g -o hw1.out hw1.mli hw1.ml hw1_test.ml

reduction : hwred.out
	./hwred.out

hwred.out : hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw1_reduction_test.ml
	ocamlc -g -o hwred.out unix.cma hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw1_reduction_test.ml

clean :
	rm *.[^m]*
