(* having "Int 4" -> "Int 4" requires use of polymorphic variants *)
(* otherwise: "Int 4" -> "EInt 4" which is really fucking ugly because of redundancy of types in expr and value *)
(* or: "int 4" -> "Int 4" where 'int' is a function that allows the user to express Val(Int(4)) and at the same time *)
(*     avoids redundancy in the expr type and eval interpreter *)
type value = [`Int of int | `Bool of bool]
type expr = [value | `Var of string | `Add of expr * expr]

let add = function `Int a,`Int b -> `Int (a+b) | _->failwith "types not accepted"
let rec eval e = match e with (* can this be function Int_ |Bool _ ... as e -> e ? *)
| `Int _ | `Bool _ -> e
| `Add (e1,e2) -> add (eval e1, eval e2)
| _ -> failwith "ho"
