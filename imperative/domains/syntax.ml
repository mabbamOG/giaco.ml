(** SYNTACTIC **)
type expr =
    (* BASE TYPES *)
    | Int of int
    | Str of string
    | Bool of bool
    | Float of float
    (* LAMBDAS *)
    | Lambda of ide*expr
    | RecLambda of ide*ide*expr
    | Rec of ide*expr
    (* PROCEDURES *)
    | Proc of ide list*com (* used by Call *)
    (* CONTROL FLOW *)
    | IfThenElse of expr*expr*expr
    (* DEREFERENCE & BLOCKS *)
    | Var of ide (*get environment id value*)
    | LetIn of ide*expr*expr (* functional block *)
    | Val of ide (* get store id value*)
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
    (* BLOCKS AND PROCEDURES *)
    | Block of dec*com (* dec must be local to block *)
    | Call of expr*expr list
    (* CONTROL FLOW *)
    | While of expr*com
    | CIfThen of expr*com
    | CIfThenElse of expr*com*com
    | CSeq of com*com
    | CSkip
    | Reflect of string
and dec =
    | New of ide*expr
    | DSeq of dec*dec
    | DSkip
and prog = Prog of dec*com
