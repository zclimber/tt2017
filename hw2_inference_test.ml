open Hw1
open Hw2_unify
open Hw2_inference

type test_for_alpha_eq = {lmd1 : string; lmd2 : string; ans : bool};;

let tests_fae = 
  [{lmd1 = "(x)"; lmd2 = "(y)"; ans = false};
   {lmd1 = "x y"; lmd2 = "x y"; ans = true};
   {lmd1 = "\\x.x y"; lmd2 = "\\y.y y"; ans = false};
   {lmd1 = "\\x1.x1"; lmd2 = "\\y1.y1"; ans = true};
   {lmd1 = "(\\x.x)(z)(w)"; lmd2 = "(\\y.y)(z)(w)"; ans = true};
   {lmd1 = "\\x1.\\x2.\\x3.\\x4.x1 x2 x3 x4";
    lmd2 = "\\y1.\\y2.\\y3.\\y4.y1 y2 y3 y4"; ans = true};
   {lmd1 = "\\x1.\\x2.\\x3.\\x4.x4 x2 x3 x1";
    lmd2 = "\\y1.\\y2.\\y3.\\y4.y1 y2 y3 y4"; ans = false};
   {lmd1 = "\\x1.\\x2.x1 x2";  lmd2 = "\\y1.\\y2.y2 y1"; ans = false}];;

let rec str_of_s_t s_type = match s_type with
  | S_Elem x -> x
  | S_Arrow (f, x) -> "(" ^ (str_of_s_t f) ^ " -> " ^ str_of_s_t x ^ ")"

let type_tester t =
  print_string ("Alpha-eq " ^ string_of_bool t.ans ^ "\n");
  let prnt s =
    let res = infer_simp_type (lambda_of_string s) in
    let res_s = match res with
      | Some (_, st) -> str_of_s_t st
      | None -> "no simple type"
    in 
      print_string (s ^ " --->\n" ^ res_s ^ "\n")
  in prnt t.lmd1; prnt t.lmd2

let rec tester pred name l ind cor incor = 
  match l with 
    | [] ->  print_newline()
    | x::xs -> pred x; print_string "\n"; tester pred name xs ind cor incor;;

tester (type_tester) "is_alpha_equivalent" tests_fae 1 0 0;;

let rec string_of_hm_type hm_type = 
  let rec impl a =
    match a with
      | HM_Elem(s)-> s
      | HM_Arrow(x, y) -> "(" ^ impl x ^ " -> " ^ impl y ^ ")"
      | HM_ForAll(s, y) -> "V " ^ s ^ " " ^ impl y
  in
    impl hm_type

let test123 t = 
  let ans1 = algorithm_w t in
    match ans1 with 
      | Some (l, s) -> print_string (string_of_hm_type s ^ "\n")
      | None -> print_string ""

let tt1 = HM_Abs("x", HM_Var("x"));;
let tt2 = HM_Let("w", HM_Abs("f", HM_Abs("x", HM_App(HM_Var("f"), HM_App(HM_Var("f"), HM_Var("x"))))), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_Var("w"))))))))))))));; 

test123 tt1;;
test123 tt2;;
