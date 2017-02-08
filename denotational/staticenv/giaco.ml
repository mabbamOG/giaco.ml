(***********************************************)
(*****************SYNTAX************************)
(***********************************************)


(* internal language type system *)
type value = Int of int | Bool of bool
(* type variable = string *) (* NOT TO BE USED UNLESS I  HAVE A CONSTRUCTOR *)

(* external language parser through ocaml type constructors *)
type expr =
| Val of value
(*| Var of variable*)
| Var of string
| Add of expr*expr
(** external aliases to allow for the user input of types, basically a hack *)
let int x= Val(Int(x))
let bool b = Val(Bool(b))

(***********************************************)
(*****************SEMANTICS*********************)
(***********************************************)

(* internal implementation for the language, expects evaluated expressions *)
let add = function
| Int a, Int b -> Int (a+b)
| _ -> failwith "add operation not supported with these types"

(* interpreter for the language, environment is static and global *)
(** type is (variable->value) -> expr -> value *)
let rec eval env = function
| Val a -> a
| Var id -> env id
| Add (e1,e2)-> add (eval env e1,eval env e2)

(***********************************************)
(*****************TESTING***********************)
(***********************************************)
;;print_string "---------------commence phase of testing----------------"

let test_env = function
| "a" -> Int 3
| "b" -> Int 4
| "ciao" -> Int 42
| _ -> failwith "variable not bound"

;;eval test_env (int 5)
;;eval test_env (bool true)
;;eval test_env (Add(int 5, int 5))
;;print_string "---------------end phase of testing----------------"

(***********************************************)
(*****COMMANDDDDDDREFLECTION********************)
(***********************************************)
(*type typo = RInt of string*Int | *)
(* 1. lex string into list of tokens *)
(* 2. parse commands by splitting @ ; (?) and then read first word *)
(* 3. profit *)
(* 4. ???? *)
