(*Gabriello Lima*)
(*Part 1, #1*)
let rec pow x n = 
  let sum = x in
  if n > 0 then sum * pow x (n-1)
  else 1;;    
     

let rec float_pow (x : float) (n: int )= 
  let sum = x in
  if n > 0 then sum *. float_pow x (n-1)
  else 1.0;;  



(*Part 1, #2*)
let compress = 
  let rec append newList l =
    match l with 
    (*[] -> newList*)
    | [] -> List.rev newList
    | [x] -> append (x::newList) []
    | h::(x::t) -> if h = x then append (newList) (x::t)
        else append (h::newList) (x::t) in
  fun l -> append [] l;;



(*Part 1, #3*)
let remove_if list (fn : int -> bool)= 
  let rec append newList l fn =
    match l with 
    | [] -> List.rev newList
    | [x] -> if fn x then append (newList) [] fn else append (x::newList) [] fn
    | h::(t) -> if fn h then append (newList) (t) fn
        else append (h::newList) (t) fn in 
  append [] list fn
;;
    


(*Part 1, #4*)
let boolrange a b c = (*a is lowerbound, b input, c upper bound*)
(b >= a) && (b < c) 

let slice l a b = 
  let rec loop newList l count = 
    match l with 
    | [] -> List.rev newList
    | [x] -> if (boolrange a count b) then loop (x::newList) []  (count+1)
        else loop (newList) [] (count+1) 
    | h::t -> if (boolrange a count b) then loop (h::newList) (t)  (count+1)
        else loop (newList) (t) (count+1)   in
  loop [] l 0;;



(*Part 1, #5*)
let equivs (fn: int -> int -> bool) l = 
  let rec append head trueList falseList l =
    match l with 
    | [] ->
        let trueList = List.rev trueList in 
        let falseList = List.rev falseList in
        [trueList]@[falseList]
    | [x] -> 
        if fn head x then append head (x::trueList) falseList []
        else  append head (trueList) ([x]@falseList) []
    | h::(t) ->
        if fn head h then append head (h::trueList) falseList (t) 
        else  append head (trueList) ([h]@falseList) (t) in
  append (List.hd l) ([]) ([]) l 
;;
      



(*Part 1, #6*)
let isPrime n = 
  let rec divideby divisor count = 
    if divisor > n then count else 
    if n mod divisor = 0 then divideby(divisor + 1) (count+1)
    else divideby (divisor + 1) (count) in
  divideby 1 0;;


let goldbachpair x = 
  let rec tupleLoop i l u = 
    if isPrime l = 2 && isPrime u = 2 
    then (l, u)
    else tupleLoop (i+1) (i+1) (x- (i+1)) in 
  tupleLoop 1 1 (x-1) 
;;



(*Part 1, #7*)
let check_equiv l = 
  let listSize = List.length l in
  let rec counting l numTrue = 
    match l with 
    | [] -> listSize = numTrue
                  (*if listSize = numTrue 
                 then true (*counting true [] numTrue*) 
                 else false(*counting false [] numTrue*)*)
    | [x1, x2] -> if x1 = x2 
        then counting [] (numTrue + 1)
        else counting [] numTrue
    | (x1,x2)::(t) -> if x1 = x2 
        then counting (t) (numTrue + 1)
        else counting (t) (numTrue) in
  counting l 0
;; 
let equiv_on (f: int->int) (g:int->int) = 
  let rec append newList l =
    match l with 
    | [] ->
            (*List.rev newList*)
        check_equiv newList
    | [x] ->
        let x1 = f x in
        let x2 = g x in
        append ((x1,x2)::newList) []
    | h::(t) ->
        let x1 = f h in
        let x2 = g h in
        append ((x1,x2)::newList) (t) in
  fun l -> append [] l 
;;




(*Part 1, #8*)
let pairwisefilter (cmp: 'a -> 'a -> 'a) list =
  let rec append newList oldList =
    match oldList with 
    | [] ->
        List.rev newList
    | [x] -> 
        append newList []
    | h::(t) ->
        append ((cmp h (List.hd t))::newList) (List.tl t) in
  append [] list
;;






(*Part 1, #9*)
let polynomial list xvalue = 
  let rec append l sum =
    match l with 
    | [] -> 0
    | [x1, x2] -> sum + x1 * pow xvalue x2 + append [] sum
    | (x1,x2)::(t) -> sum + x1 * pow xvalue x2 + append t sum in
  append list 0;;
    






(*Part 1, #10*)
  let rec powersetHelper head newList iterateList = 
    match iterateList with 
    | [] ->
        newList
    | [x] -> (*Last value in iterateList*)
        powersetHelper head ((head::[x])::newList) []
    | h::(t) -> 
        powersetHelper head ((head::[h])::newList) (t)
  ;;
  let rec powersetSecondHelper head newList iterateList = 
    match iterateList with 
    | [] ->
        newList
    | [x] -> (*Last value in iterateList*)
        powersetSecondHelper head (newList) []
    | h::(t) -> 
        powersetSecondHelper head ((head::h::[List.hd t])::newList) (t)
  ;;
  let rec powersetThirdHelper newList iterateList = 
    match iterateList with 
    | [] ->
        newList
    | [x] -> (*Last value in iterateList*)
        powersetThirdHelper (newList) [] 
    | _::_::[] -> powersetThirdHelper (newList) []
    | a::b::c::d -> 
        powersetThirdHelper ((a::b::c::[List.hd d])::newList) []
  ;;
  
  let powerset list = 
    let rec append newList l =
      match l with 
      | [] -> 
          if (List.length list > 4 )then
            let newList = 
              powersetSecondHelper (List.nth list 2) newList (List.tl (List.tl(List.tl list))) in
            let newList = powersetSecondHelper (List.hd list) newList (List.tl list) in
            let newList = powersetSecondHelper (List.hd (List.tl list)) newList (List.tl (List.tl list)) in 
            let newList = powersetThirdHelper newList (List.tl list)in 
            let newList = powersetThirdHelper newList (list)in 
            let newList = list::newList in 
            List.rev newList
          else if List.length list > 3 then
            let newList = powersetSecondHelper (List.hd list) newList (List.tl list) in
            let newList = powersetSecondHelper (List.hd (List.tl list)) newList (List.tl (List.tl list)) in
            let newList = list::newList in
            List.rev newList 
          else
            let newList = list::newList in
            List.rev newList
          (*If the list is empty, we reverse it*)
      | [x] -> (*If its the last value in the true list... add Full set*) 
          append (newList) []
      | h::(t) ->
          let newList = powersetHelper h newList t in
          append (newList) (t) in
    append [[]] list (*Empty set is accounted for here*)
  ;; 