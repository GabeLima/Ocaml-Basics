(*Gabriello Lima *)
(*Part 3*)
type expr = 
  Empty
| Const of int
| Var of string
| Plus of node
| Minus of node
| Div of node
| Mult of node 
and 
node = 
{ arg1: expr;
  arg2: expr;
}
;;
let evaluate expr = 
let rec evaluation arith_expr = 
  match arith_expr with
  |Empty -> 0 
  |Const x -> x 
  |Var y -> 1 (*I only put this for exhaustive purposes*)
  |Plus {arg1 = x; arg2 = y}-> evaluation x + evaluation y
  |Minus {arg1 = x; arg2 = y}-> evaluation x - evaluation y
  |Mult {arg1 = x; arg2 = y}-> evaluation x * evaluation y
  |Div {arg1 = x; arg2 = y}-> evaluation x / evaluation y
in
evaluation expr
;;