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
let add = function
| Int a, Int b -> Int (a+b)
| _ -> failwith "plus operation not supported with these types"

(* interpreter for the language, environment is static and global *)
(** type is (variable->value) -> expr -> value *)
let rec eval env = function
| Val a -> a
| Var id -> env id
| Plus (e1,e2)-> add (eval env e1,eval env e2)

(***********************************************)
(*****************TESTING***********************)
(***********************************************)

let test_env = function
| "a" -> Int 3
| "b" -> Int 4
| "ciao" -> Int 42
| _ -> failwith "variable not bound"

let _ = eval test_env (int 5)
let _ = eval test_env (bool true)
let _ = eval test_env (Plus(int 5, int 5))
