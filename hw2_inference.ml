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

let s_count = ref 0

let s_name () =
  let value = !s_count in
    s_count := value + 1;
    "st" ^ string_of_int value

type simp_type = 
    | S_Elem of string 
    | S_Arrow of simp_type * simp_type

let mkvar x = Hw2_unify.Var x

let new_var () = mkvar (s_name ())

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
  s_count := 0;
  let sys, top = constraints lam in
    match Hw2_unify.solve_system sys with
      | Some solution ->
          Some (List.map (fun (a, b) -> (a, simp_type_of_term b)) solution,
                simp_type_of_term (Hw2_unify.apply_substitution solution top))
      | None -> None

(*val algorithm_w : hm_lambda -> ((string * hm_type list) * hm_type) option*)

type hm_lambda = 
    | HM_Var of string 
    | HM_Abs of string * hm_lambda 
    | HM_App of hm_lambda * hm_lambda 
    | HM_Let of string * hm_lambda * hm_lambda

type hm_type = 
    | HM_Elem of string 
    | HM_Arrow of hm_type * hm_type 
    | HM_ForAll of string * hm_type

let hm_count = ref 0

let hm_name () =
  let value = !hm_count in
    hm_count := value + 1;
    "hmt" ^ string_of_int value

let new_elem () = HM_Elem (hm_name ())

let rec free_types hm_type =
  match hm_type with
    | HM_Elem(s) -> SS.singleton s
    | HM_Arrow(x, y) -> SS.union (free_types x) (free_types y)
    | HM_ForAll(s, x) -> SS.remove s (free_types x)

let hm_subst subst in_type =
  let rec subst_rec in_type defined =
    match in_type with
      | HM_Elem(s) -> if (SS.mem s defined || not (SM.mem s subst))
          then in_type
          else (SM.find s subst)
      | HM_Arrow(x, y) -> 
          HM_Arrow(subst_rec x defined, subst_rec y defined)
      | HM_ForAll(s, x) ->
          HM_ForAll(s, subst_rec x (SS.add s defined))
  in subst_rec in_type SS.empty

let hm_subst_map subst type_map =
  SM.map (fun tp -> hm_subst subst tp) type_map

let combine_subst subst1 subst2 =
  let new_s2 = hm_subst_map subst1 subst2 in
  let union tp1 tp2 =
    match tp1 with
      | Some s -> tp1
      | None -> tp2
  in
    SM.merge (fun key tp1 tp2 -> union tp2 tp1) subst1 new_s2

let rec rem_forall hm_type =
  match hm_type with
    | HM_ForAll(s, x) -> hm_subst (SM.singleton s (new_elem ())) (rem_forall x)
    | _ -> hm_type

let add_forall hm_type types =
  let all_free_types = SM.fold (fun s x set -> SS.union set (free_types x)) types SS.empty in
  let unchecked_free_types = (SS.diff (free_types hm_type) all_free_types) in
    SS.fold (fun s x -> HM_ForAll(s, x)) unchecked_free_types hm_type

exception WExc of string 

let rec term_of_hm_type hm_type =
  match hm_type with
    | HM_Elem(a) -> Hw2_unify.Var(a)
    | HM_Arrow(a, b) -> Hw2_unify.Fun("->", [(term_of_hm_type a);(term_of_hm_type b)])
    | _ -> failwith "Cannot convert hm_type to term"

let rec hm_type_of_term term = 
  match term with
    | Hw2_unify.Var(a) -> HM_Elem(a)
    | Hw2_unify.Fun(name, [l;r]) -> HM_Arrow(hm_type_of_term l, hm_type_of_term r)
    | _ -> failwith "Cannot convert term to hm_type"

let map_of_alist alist =
  List.fold_left (fun map (key, value) -> SM.add key value map) SM.empty alist

let rec alg_w_rec types hm_lam =
  match hm_lam with
    | HM_Var(s) -> if SM.mem s types
        then (rem_forall (SM.find s types), SM.empty)
        else raise (WExc "Free variable")
    | HM_Abs(s, x) ->
        let new_hm = new_elem () in
        let (x_hm, x_sub) = alg_w_rec (SM.add s new_hm types) x in
          (HM_Arrow(hm_subst x_sub new_hm, x_hm), x_sub)
    | HM_Let(s, x, y) -> 
        let (x_hm, x_sub) = alg_w_rec types x in
        let new_types = hm_subst_map x_sub types in
        let (y_hm, y_sub) = alg_w_rec (SM.add s (add_forall x_hm new_types) new_types) y in
          (y_hm, combine_subst y_sub x_sub)
    | HM_App(x, y) -> 
        (let (x_hm, x_sub) = alg_w_rec types x in
         let (y_hm, y_sub) = alg_w_rec (hm_subst_map x_sub types) y in
         let new_hm = new_elem () in 
           match solve_system [(term_of_hm_type (hm_subst y_sub x_hm), term_of_hm_type(HM_Arrow(y_hm, new_hm)))] with
             | None -> raise (WExc "Unsolvable system in application")
             | Some ans -> 
                 let ans_sub = SM.map (hm_type_of_term) (map_of_alist ans) in
                 let all_types = combine_subst ans_sub (combine_subst y_sub x_sub) in
                   (hm_subst all_types new_hm, all_types) )

let algorithm_w hm_lam =
  let start_types = SM.empty
  in try
      let (hm, types) = alg_w_rec start_types hm_lam in
        Some (SM.bindings types, hm)
    with (WExc e) -> None

