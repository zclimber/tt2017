.PHONY : hw1 reduction unify clean

hw1 : hw1.out
	./hw1.out

hw1.out : hw1.mli hw1.ml hw1_test.ml
	ocamlc -g -o hw1.out hw1.mli hw1.ml hw1_test.ml

reduction : hwred.out
	./hwred.out

hwred.out : hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw1_reduction_test.ml
	ocamlc -g -o hwred.out unix.cma hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw1_reduction_test.ml

unify : hw2_uni.out
	./hw2_uni.out

hw2_uni.out : hw2_unify.mli hw2_unify.ml hw2_unify_test.ml
	ocamlc -g -o hw2_uni.out hw2_unify.mli hw2_unify.ml hw2_unify_test.ml

clean :
	rm *.[^m]*
