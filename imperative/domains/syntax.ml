(** SYNTACTIC **)
type expr =
    (* BASE TYPES *)
    | Int of int
    | Str of string
    | Bool of bool
    | Float of float
    | Lambda of ide*expr
    (* CONTROL FLOW *)
    | IfThenElse of expr*expr*expr (* not the same as cmd *)
    (* DEREFERENCE *)
    | Var of ide (*get environment variable value*)
    | LetIn of ide*expr*expr (* denotational environment extension, a bit different from normal assignment*)
    | Val of ide (*dereference environment variable value*)
    (* FUNCTIONS *)
    | Plus of expr*expr
    | Multiply of expr*expr
    | Apply of expr*expr
    | Equals of expr*expr
    | Greater of expr*expr
    | Not of expr
    | Or of expr*expr
    | And of expr*expr
    | Len of expr
    | Sub of expr*expr*expr
and com =
    (* SIDE EFFECT *)
    | Assign of ide*expr
    (* CONTROL FLOW *)
    | While of expr*com
    | CIfThen of expr*com (* more stateful to have com list? or does CSeq take care?*)
    | CIfThenElse of expr*com*com
    | CSeq of com*com
    | Skip
    (*| Reflect of string*)
and dec =
    | New of ide*expr
    | DSeq of dec*dec
and prog = Prog of dec*com
