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

type env = ide -> value (*can otherwise be (ide->value) list ??????*)


(** SYNTACTIC **)
type expr =
    (** BASE TYPES **)
    | Int of int
    | Str of string
    | Bool of bool
    | Float of float
    | Lambda of ide*expr
    (*| IfThenElse expr*expr*expr*)

    (** DEREFERENCING **)
    | Var of ide (*get me this environment variable's value*)
    (*| In of ide*expr*expr (*denotational environment extension, a bit different from normal assignment*)*)
    (*| Deref/Val of ide (*get me the value of the memory loc this environment variable is holding*)*)

    (** FUNCTIONS **)
    | Plus of expr*expr
    | Multiply of expr*expr
    | Apply of expr*expr
    | Equals of expr*expr
    | Greater of expr*expr
    | Not of expr
    | Or of expr*expr
    | And of expr*expr
