type peano = Z | S of peano;; (* Типы необходимо копировать в реализацию *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

let rec peano_of_int x = 
  if x > 0
  then S (peano_of_int (x - 1))
  else Z

let rec int_of_peano p = 
  match p with
      Z -> 0
    | S x -> 1 + int_of_peano x

let inc x = S x

let add x y = 
  let rec add2 x y =
    match (x, y) with
      | (x, Z) -> x
      | (x, S z) -> inc (add2 x z)
  in
    match (x, y) with
        (Z, y) -> y
      | (x, y) -> add2 x y

let rec sub x y = 
  match (x, y) with
      (Z, y) -> Z
    | (x, Z) -> x
    | (S a, S b) -> sub a b

let mul x y = 
  let rec mul2 x y =
    match (x, y) with
        (x, Z) -> Z
      | (x, S z) -> add (mul2 x z) x
  in
    match (x, y) with 
        (Z, y) -> Z
      | (x, y) -> mul2 x y

let power x y = 
  let rec power2 x y =
    match (x, y) with
        (x, Z) -> S Z
      | (x, S z) -> mul (power2 x z) x
  in
    match (x, y) with
      | (x, Z) -> S Z
      | (Z, y) -> Z
      | (S Z, y) -> S Z
      | (x, y) -> power2 x y

let rev x =
  let rec rev_acc acc x =
    match (acc, x) with
        (acc, []) -> acc
      | (acc, x::xs) -> rev_acc (x::acc) xs
  in
    rev_acc [] x

let merge_sort x = 
  let rec merge_rec x sz =
    if sz <= 1
    then x
    else
      let rec split x i =
        match (x, i) with
            (x, 0) -> ([], x)
          | ([], i) -> failwith "Split index bigger than list"
          | (x::xs, i) -> let (l, r) = split xs (i - 1) in (x::l, r)
      in
      let rec merge x y = 
        match (x, y) with
            ([], y) -> y
          | (x, []) -> x
          | (xx::xs, yy::ys) ->
              if xx <= yy
              then xx :: (merge xs y)
              else yy :: (merge x ys)
      in
      let (l, r) = split x (sz / 2) in
        merge (merge_rec l (sz / 2)) (merge_rec r (sz - sz / 2))
  in merge_rec x (List.length x)

let rec string_of_lambda x =
  match x with
    | Var v -> v
    | App (x,y) -> "(" ^ string_of_lambda x ^ " " ^ string_of_lambda y ^ ")"
    | Abs (v,x) -> "(\\" ^ v ^ "." ^ string_of_lambda x ^ ")"

open Genlex

let lambda_of_stream stream =
  let next () = Stream.next stream in
  let peek () = Stream.peek stream in
  let fail s = failwith ("Unexpected token" ^ s) in
  let tkn s = if(next () <> Kwd s) then fail s in

  let rec parse_lambda () =
    match next () with
        Kwd "(" -> 
          let lam = parse_start_lambda () in tkn ")" ; Some lam
      | Kwd "\\" -> Some (parse_abstract ())
      | Ident s -> Some (Var s)
      | _ -> None

  and parse_start_lambda () =
    match parse_lambda () with
        None -> fail "?"
      | Some lam -> try_appl lam

  and parse_abstract () =
    match next () with
        Ident s -> 
          (match tkn "."; parse_lambda () with
              Some lam -> Abs (s, lam)
            | None -> fail "?")
      | _ -> fail "?"

  and try_appl func =
    match peek () with
        None -> func
      | Some Kwd ")" -> func
      | _ -> match parse_lambda() with
          None -> fail "?"
        | Some lam -> try_appl (App (func, lam))

  in parse_start_lambda ()


let lexer = make_lexer ["\\";".";"(";")"]

let lambda_of_string x = lambda_of_stream (lexer (Stream.of_string x))
