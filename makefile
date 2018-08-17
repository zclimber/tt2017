.PHONY : hw1 reduction unify infer all clean

all : hw1.out hw1red.out hw2uni.out hw2inf.out

hw1 : hw1.out
	./hw1.out

hw1.out : hw1.mli hw1.ml hw1_test.ml
	ocamlc -g -o hw1.out hw1.mli hw1.ml hw1_test.ml

reduction : hw1red.out
	./hw1red.out

hw1red.out : hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw1_reduction_test.ml
	ocamlc -g -o hw1red.out hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw1_reduction_test.ml

unify : hw2uni.out
	./hw2uni.out

hw2uni.out : hw2_unify.mli hw2_unify.ml hw2_unify_test.ml
	ocamlc -g -o hw2uni.out hw2_unify.mli hw2_unify.ml hw2_unify_test.ml

infer : hw2inf.out
	./hw2inf.out

hw2inf.out : hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw2_unify.mli hw2_unify.ml hw2_inference.mli hw2_inference.ml hw2_inference_test.ml
	ocamlc -g -o hw2inf.out hw1.mli hw1.ml hw1_reduction.mli hw1_reduction.ml hw2_unify.mli hw2_unify.ml hw2_inference.mli hw2_inference.ml hw2_inference_test.ml

clean :
	rm *.[^m]*
