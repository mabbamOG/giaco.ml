(** ? **)
type ide = string

(** SEMANTIC **)
type value =
    | EInt of int
    | EStr of string
    | EBool of bool
    | EFloat of float
    | ELambda of (value->value)
    | Unbound (* really necessary? *)

type env = ide -> value

(** SYNTACTIC **)
type expr =
    (* BASE TYPES *)
    | Int of int
    | Str of string
    | Bool of bool
    | Float of float
    | Lambda of ide*expr

    (* CONTROL FLOW *)
    | IfThenElse of expr*expr*expr (* expr if =/ cmd if *)

    (* DEREFERENCE *)
    | Var of ide (*get me this environment variable's value*)
    | LetIn of ide*expr*expr (* denotational environment extension, a bit different from normal assignment*)
    (*| LetAnd of (ide*expr) list * expr*)
    (*| Deref/Val of ide (*get me the value of the memory loc this environment variable is holding*)*)

    (* FUNCTIONS *)
    | Plus of expr*expr
    | Multiply of expr*expr
    | Apply of expr*expr
    | Equals of expr*expr
    | Greater of expr*expr
    | Not of expr
    | Or of expr*expr
    | And of expr*expr
