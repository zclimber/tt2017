open Hw1

module SS = Set.Make(String)

module SM = Map.Make(String)

module IM = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

let full_det = -1

let prnt det lazy_str =
  if det <= full_det then print_string (Lazy.force lazy_str)

let rec free_set lam =
  match lam with
      Var s -> SS.singleton s
    | Abs (s, l) -> SS.remove s (free_set l)
    | App (f, x) -> SS.union (free_set f) (free_set x)

let free_vars lam = SS.elements (free_set lam)

type half_bruijn =
      Free of string
    | Bound of int
    | Abstract of half_bruijn ref
    | Apply of half_bruijn ref * half_bruijn ref

let string_of_bruijn bru =
  let rec str depth br =
    match !br with
        Free s -> s
      | Bound i -> string_of_int i
      | Abstract l -> "(\\" ^ string_of_int depth ^ "." ^ str (depth + 1) l ^ ")"
      | Apply (f, x) -> "(" ^ str depth f ^ " " ^ str depth x ^ ")"
  in str 0 bru

let bruijn_of_lambda lam =
  let rec bruijn_rec depth mp lam =
    let bru_n = bruijn_rec depth in
      match lam with
          Var s -> 
            if SM.mem s mp
            then ref (Bound (depth - SM.find s mp))
            else ref (Free s)
        | Abs (s, l) -> ref (Abstract (bruijn_rec (depth + 1) (SM.add s depth mp) l))
        | App (f, x) -> ref (Apply (bru_n mp f, bru_n mp x))
  in bruijn_rec 0 SM.empty lam

let next_id cnt =
  let value = !cnt in
    cnt := value + 1;
    "id" ^ string_of_int value

let lambda_of_bruijn bru = 
  let cnt = ref 0 in
  let rec lambda_rec depth mp bru =
    let lr = lambda_rec depth mp in
      prnt 5 (lazy (string_of_int depth ^ "d: " ^ string_of_bruijn bru ^ ";\n"));
      match !bru with
          Free s -> Var s
        | Bound i -> Var (IM.find (depth - i) mp)
        | Apply (f, x) -> App (lr f, lr x)
        | Abstract l -> let id = next_id cnt in
              Abs (id, lambda_rec (depth + 1) (IM.add depth id mp) l)
  in lambda_rec 0 IM.empty bru

let rec clone_bruijn bru =
  match !bru with
      Apply (f, x) -> ref (Apply (clone_bruijn f, clone_bruijn x))
    | Abstract l -> ref (Abstract (clone_bruijn l))
    | Bound i -> ref (Bound i)
    | Free _ -> bru

let free_to_subst what where s =
  let free_set = free_set what in
  let rec subst where bound =
    match where with
        Var v -> v <> s || SS.is_empty (SS.inter free_set bound)
      | Abs (v, l) -> v = s || subst l (SS.add v bound)
      | App (f, x) -> subst f bound && subst x bound
  in subst where SS.empty

let rec is_normal_form lam =
  match lam with
      Var _ -> true
    | Abs (_, l) -> is_normal_form l
    | App (f, x) -> match f with
        Abs _ -> false
      | _ -> is_normal_form f && is_normal_form x

let is_alpha_equivalent l1 l2 =
  let br1 = bruijn_of_lambda l1 in
  let br2 = bruijn_of_lambda l2 in
    br1 = br2

let bruijn_replace what where_x =
  let depth_store = ref (IM.singleton 0 what) in 
  let rec redepth_rec new_depth node depth = 
    match !node with
      | Free f -> node
      | Bound i -> if i > depth then ref (Bound (i + new_depth)) else node
      | Abstract l -> 
          let new_l = redepth_rec new_depth l (depth + 1) in
            if new_l != l then ref (Abstract new_l) else node
      | Apply (f, x) ->
          let new_f = redepth_rec new_depth f depth in
          let new_x = redepth_rec new_depth x depth in
            if (new_f != f) || (new_x != x) then ref (Apply (new_f, new_x)) else node
  in 
  let redepth new_depth =
    (if not (IM.mem new_depth !depth_store) 
     then depth_store := IM.add new_depth (redepth_rec new_depth what 0) !depth_store);
    IM.find new_depth !depth_store
  in 
    prnt 3 (lazy ("repl " ^ string_of_bruijn what ^ "->\n"));
    let rec replace depth where =
      prnt 5 (lazy ("r " ^ string_of_int depth ^ "d " ^ string_of_bruijn where ^ ";\n"));
      match !where with
          Bound i -> 
            if i = depth + 1 
            then where := !(redepth depth) 
            else if i > depth + 1
            then where := Bound (i - 1)
        | Abstract l -> replace (depth + 1) l
        | Apply (f, x) -> let r = replace depth in r f; r x
        | _ -> ()
    in replace 0 where_x

let rec bruijn_step_reduce lam =
  let ret_this res ret = match res with
      Some _ -> Some ret
    | None -> None 
  in 
    match !lam with
        Abstract l -> ret_this (bruijn_step_reduce l) lam
      | Apply (f, x) ->
          (match !f with
              Abstract l -> 
                lam := !(clone_bruijn l);
                bruijn_replace x lam; 
                Some lam
            | _ -> match bruijn_step_reduce f with
                Some s -> Some lam;
              | None -> ret_this (bruijn_step_reduce x) lam)
      | _ -> None

let normal_beta_reduction lam =
  let bru = bruijn_of_lambda lam in
    prnt 1 (lazy (string_of_bruijn bru ^ "\n")); 
    let result = 
      match bruijn_step_reduce bru with
          Some red -> red
        | None -> bru
    in 
      prnt 1 (lazy (string_of_bruijn bru ^ "\n")); lambda_of_bruijn result

let reduce_to_normal_form lam =
  let count = ref 0 in
  let bru = bruijn_of_lambda lam in
  let rec full_reduce bru = 
    (if !count mod 100 = 0 then prnt 0 (lazy (string_of_int !count ^ "norm\n")));
    count := !count + 1;
    prnt 1 (lazy "step ");
    prnt 1 (lazy (string_of_bruijn bru ^ "\n"));
    prnt 2 (lazy ("lam " ^ string_of_lambda (lambda_of_bruijn bru) ^ "\n"));
    flush Pervasives.stdout;
    match bruijn_step_reduce bru with
      | None -> bru
      | Some reduced -> full_reduce reduced
  in lambda_of_bruijn (full_reduce bru)
