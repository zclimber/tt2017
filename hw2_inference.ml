
#load "hw1.cmo";;
#load "hw1_reduction.cmo";;
#load "hw2_unify.cmo";;


open Hw1
open Hw1_reduction
open Hw2_unify

type simp_type = 
    | S_Elem of string 
    | S_Arrow of simp_type * simp_type

type hm_lambda = 
    | HM_Var of string 
    | HM_Abs of string * lambda 
    | HM_App of lambda * lambda 
    | HM_Let of string * lambda * lambda

type hm_type = 
    | HM_Elem of string 
    | HM_Arrow of hm_type * hm_type 
    | HM_ForAll of string * hm_type

let infer_simp_type lam =
  None

let algorithm_w hm_lam =
  None
