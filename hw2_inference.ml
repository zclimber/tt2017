(*
#load "hw1.cmo";;
#load "hw1_reduction.cmo";;
#load "hw2_unify.cmo";;
*)

open Hw1
open Hw1_reduction
open Hw2_unify

module SS = Set.Make(String)

module SM = Map.Make(String)

let count = ref 0

let s_type () =
  let value = !count in
    count := value + 1;
    "st" ^ string_of_int value

type simp_type = 
    | S_Elem of string 
    | S_Arrow of simp_type * simp_type

let mkvar x = Hw2_unify.Var x

let new_var () = mkvar (s_type ())

let mkfun f x = Hw2_unify.Fun ("", [f;x])

let rec term_of_simp_type t = 
  match t with
    | S_Elem(x) -> mkvar x
    | S_Arrow(f, x) -> mkfun (term_of_simp_type f) (term_of_simp_type x)

let rec simp_type_of_term t =
  match t with
    | Hw2_unify.Var(x) -> S_Elem(x)
    | Hw2_unify.Fun(_, [f;x]) -> S_Arrow(simp_type_of_term f, simp_type_of_term x)
    | _ -> failwith "Wrong term from simple type"

let constraints lam =
  let frees = List.fold_right (fun x mp -> SM.add x (new_var ()) mp) (Hw1_reduction.free_vars lam) SM.empty in 
  let rec collect lam bound =
    match lam with
      | Hw1.Var x -> ([], SM.find x bound)
      | Hw1.Abs (x, l) ->
          let x_var = new_var () in
          let sys, top = collect l (SM.add x x_var bound) in
            (sys, mkfun x_var top)
      | Hw1.App (f, x) ->
          let sys1, func = collect f bound in
          let sys2, arg = collect x bound in
          let res = new_var () in
            (sys1 @ sys2 @ [(func, mkfun arg res)], res)
  in collect lam frees

let infer_simp_type lam =
  count := 0;
  let sys, top = constraints lam in
    match Hw2_unify.solve_system sys with
      | Some solution ->
          Some (List.map (fun (a, b) -> (a, simp_type_of_term b)) solution,
                simp_type_of_term (Hw2_unify.apply_substitution solution top))
      | None -> None

type hm_lambda = 
    | HM_Var of string 
    | HM_Abs of string * lambda 
    | HM_App of lambda * lambda 
    | HM_Let of string * lambda * lambda

type hm_type = 
    | HM_Elem of string 
    | HM_Arrow of hm_type * hm_type 
    | HM_ForAll of string * hm_type

(*val algorithm_w : hm_lambda -> ((string * hm_type list) * hm_type) option*)

let algorithm_w hm_lam =
  None
