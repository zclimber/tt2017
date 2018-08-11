open Hw1;;

let int_peano_int x =
  for i = 0 to x do
    let back = int_of_peano (peano_of_int i) in
      if back <> i  then print_string ("Int to Peano to Int from "
                                       ^ string_of_int i ^ " produced "
                                       ^ string_of_int back)
  done;
  print_string "Int to Peano to Int done!\n"


let peano_int_peano x =
  for i = 0 to x do
    let p = peano_of_int i in
    let back = peano_of_int (int_of_peano p) in
      if back <> p  then print_string ("I to P to I to P from "
                                       ^ string_of_int i ^ " is wrong ")
  done;
  print_string "Peano to Int to Peano done!\n"

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
      let b = pow a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a)

let peano_add i j =
  for a = 0 to i do
    for b = 0 to j do
      let res = add (peano_of_int a) (peano_of_int b) in
      let res_int = int_of_peano res in
        if res_int <> a + b then Format.printf "@[%d + %d evaluated to %d\n" a b res_int
    done
  done;
  print_string "Peano add done!\n";;

let peano_add i j =
  for a = 0 to i do
    for b = 0 to j do
      let res = add (peano_of_int a) (peano_of_int b) in
      let res_int = int_of_peano res in
        if res_int <> a + b then Format.printf "@[%d + %d evaluated to %d\n" a b res_int
    done
  done;
  print_string "Peano add done!\n";;

let peano_sub i j =
  for a = 0 to i do
    for b = 0 to a do
      let res = sub (peano_of_int a) (peano_of_int b) in
      let res_int = int_of_peano res in
        if res_int <> a - b then Format.printf "@[%d - %d evaluated to %d\n" a b res_int
    done
  done;
  print_string "Peano sub done!\n";;

let peano_mul i j =
  for a = 0 to i do
    for b = 0 to j do
      let res = mul (peano_of_int a) (peano_of_int b) in
      let res_int = int_of_peano res in
        if res_int <> a * b then Format.printf "@[%d * %d evaluated to %d\n" a b res_int
    done
  done;
  print_string "Peano mul done!\n";;

let peano_pow i j =
  for a = 0 to i do
    for b = 0 to j do
      let res = power (peano_of_int a) (peano_of_int b) in
      let res_int = int_of_peano res in
        if res_int <> (pow a b) then Format.printf "@[%d ** %d evaluated to %d\n" a b res_int
    done
  done;
  print_string "Peano pow done!\n";;

let init_list n ~f =
  if n < 0 then raise (Invalid_argument "init");
  let rec loop i accum =
    if i = 0 then accum
    else loop (i-1) (f (i-1) :: accum)
  in
    loop n []
;;

let rand_list len =
  let just_rand _ = Random.int (1000000000) in
    init_list len ~f:(just_rand)

let reverse_list i = 
  for a = 0 to i do
    let lst = rand_list a in
      if (rev lst) <> (List.rev lst) then print_string "Wrong reverse\n"
  done;
  print_string "Reverse done!\n";;

let sort_list i = 
  for a = 0 to i do
    let lst = rand_list a in
      if (merge_sort lst) <> (List.sort compare lst) then print_string "Wrong sort\n"
  done;
  print_string "Sort done!\n";;

int_peano_int 100;;
peano_int_peano 100;;
peano_add 10 10;;
peano_sub 10 10;;
peano_mul 10 10;;
peano_pow 15 5;;

reverse_list 100;;
sort_list 100;;


print_string "\n";
print_string (string_of_lambda (lambda_of_string "(x y)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.x")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.x y")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(x)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(((((((\\y.y)))))))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z))(\\x.\\y.((x y)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.x f y f")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "x1")); print_string "\n";; 
print_string (string_of_lambda (lambda_of_string "(\\x.x) (\\x.x) x")); print_string "\n";; 
