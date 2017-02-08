(* externally written *)
(* having "Int 4" -> "4" requires use of Generic Algebraic Datatypes *)
type 'a expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Equal : int expr * int expr -> bool expr

let rec eval
  : type a. a expr -> a
  = function
    | Int i -> i
    | Bool b -> b
    | Add (a, b) -> eval a + eval b
    | If (a,b,c) -> if eval a then eval b else eval c
    | Equal (a,b) -> eval a = eval b


let () =
  assert (eval
      (If (Equal (Int 2, Add (Int 1, Int 1)),
         Bool true, Bool false)));
  assert (eval
      (Add (Int 1, Add (Int 1, Int 40))) = 42);
  ()
