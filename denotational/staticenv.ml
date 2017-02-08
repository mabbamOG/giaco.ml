(***********************************************)
(*****************SYNTAX************************)
(***********************************************)

(* internal language type system *)
type value = Int of int | Bool of bool
type variable = string

(* external language parser through ocaml type constructors *)
type expr =
| Val of value
| Var of variable
| Plus of expr*expr
(** external aliases to allow for the user input of types, basically a hack *)
let int x= Val(Int(x))
let bool b = Val(Bool(b))

(***********************************************)
(*****************SEMANTICS*********************)
(***********************************************)

(* internal implementation for the language, expects evaluated expressions *)
let plus e1 e2 = match e1,e2 with
| Int x, Int y -> Int (x+y)
| _ -> failwith "plus operation not supported with these types"

(* interpreter for the language, environment is static and global *)
(** type is (variable->value) expr -> value *)
let rec eval env = function
| Val v -> v
| Var id -> env id
| Plus (e1,e2)-> plus (eval env e1) (eval env e2)
