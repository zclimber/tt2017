type algebraic_term = Var of string | Fun of string * (algebraic_term list)

module SS = Set.Make (String)

exception NoSolution of string

let id_counter = ref 0

let next_id () =
  let value = !id_counter in
    id_counter := value + 1;
    "id" ^ (string_of_int value)

let system_to_equation x =
  let func_id = next_id() in
  let members = List.split x in
    (Fun(func_id, fst members), Fun(func_id, snd members))

let substitute what s where =
  let rec subst where =
    match where with
      | Var x -> if (x = s) then what else where
      | Fun(f, args) -> Fun(f, List.map subst args)
  in subst where

let apply_substitution x y =
  let single_subst pair where =
    substitute (snd pair) (fst pair) where  in
    List.fold_right single_subst x y

let check_solution x y = 
  let eq = system_to_equation y in
    compare (apply_substitution x (fst eq)) (apply_substitution x (snd eq)) = 0

let contains s where =
  let rec cont where =
    match where with
      | Var x -> x = s
      | Fun (f, args) -> List.exists cont args
  in cont where

let rec algebraic_term_to_string (at : algebraic_term) = 
  let rec impl a =
    match a with 
      | Var x -> x
      | Fun(f, l) -> f ^ "(" ^ impl_for_list l ^ ")" 

  and impl_for_list lt = 
    match lt with 
      | [] -> ""
      | (h::[]) -> impl h
      | (h::t) -> (impl h) ^ " " ^ (impl_for_list t)
  in
    impl at

let eqt_to_string (eqt : algebraic_term * algebraic_term) = 
  match eqt with 
    | (l, r) -> algebraic_term_to_string l ^ " = " ^ algebraic_term_to_string r

let rec sys_to_string sys =
  match sys with
    | [] -> "\n";
    | h::t -> eqt_to_string h ^ "\n" ^ sys_to_string t

let prnt num str =
  if num < 0 then print_string (Lazy.force str)

let solve_system x =
  let rec resolve sys resolved =
    prnt 5 (lazy ("\n" ^ sys_to_string sys));
    if SS.cardinal resolved = List.length sys then sys else (
      prnt 3 (lazy ("SS " ^ string_of_int (SS.cardinal resolved) ^ " "));
      prnt 3 (lazy ("sys " ^ string_of_int (List.length sys) ^ " "));
      match sys with
        | [] -> raise (NoSolution "Empty system")
        | (l, r)::tail ->
            if l = r then (prnt 3 (lazy "eq\n"); resolve tail resolved) else
              match (l, r) with
                | Var x, y ->
                    prnt 3 (lazy ("var " ^ x ^ "\n"));
                    flush stdout;
                    if contains x y then raise (NoSolution "Fourth rule abused")
                    else
                      let new_resolved = SS.add x resolved in
                      let sub = List.map (substitute y x) in
                      let (eqs_l, eqs_r) = List.split sys in
                      let new_sys = List.combine (sub eqs_l) (sub eqs_r) @ [(l, r)]
                      in resolve new_sys new_resolved
                | y, Var x -> 
                    prnt 3 (lazy ("loop " ^ x ^ "\n"));
                    flush stdout;
                    resolve (tail @ [(r, l)]) resolved
                | Fun (f, a1), Fun (g, a2) ->
                    prnt 3 (lazy ("fun " ^ f ^ "\n"));
                    flush stdout;
                    if f <> g || List.length a1 <> List.length a2
                    then raise (NoSolution "Third rule abused")
                    else resolve (tail @ List.combine a1 a2) resolved)
  in
  let to_subst sys =
    let one eq = match eq with
      | Var x, s -> (x, s)
      | _ -> failwith "Incorrect substitution"
    in List.map one sys in
    try 
      let solution = resolve x SS.empty in
        Some (to_subst solution)
    with NoSolution msg ->
      None
