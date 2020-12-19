(*Gabriello Lima *)
(*Part 2*)
type bool_expr =
  | Lit of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
          
;;

let helper_fn var1 firstBool secondBool bool_expr= 
  let rec evaluate finalAnswer bool_expr= 
    (*let (firsthalf, secondhalf) = newTuple in*)
    match bool_expr with 
    | Lit i1-> if i1 = var1 then firstBool else secondBool
    | Not i2 -> not (evaluate finalAnswer i2)
    | And (i1,i2) -> evaluate finalAnswer i1 && evaluate finalAnswer  i2
    | Or (i1,i2) ->  evaluate finalAnswer i1 || evaluate finalAnswer i2 in
  evaluate firstBool bool_expr
;;
let truth_table var1 var2 bool_expr = 
  (*let answer1 = helper_fn (Lit var1) (Lit var2) bool_expr in*)
  let answer1 = (false, false, (helper_fn var1 false false bool_expr)) in
  let answer2 = (false, true, (helper_fn var1 false true bool_expr)) in
  let answer3 = (true, false, (helper_fn var1 true false bool_expr)) in
  let answer4 = (true, true, (helper_fn var1 true true bool_expr)) in
  (answer1::answer2::answer3::[answer4]) 
;;