(** GENERIC **)
type ide = string
type loc = nativeint
let newloc: int -> loc = function max_store_size -> Random.nativeint (Nativeint.of_int max_store_size)

(** SEMANTIC **)
type efun = (dvalue->store->evalue) (*  list of parameters? *)
and evalue =
    | EInt of int
    | EStr of string
    | EBool of bool
    | EFloat of float
    | ELambda of efun
    | EUnbound
and dvalue =
    | DLoc of loc
    | DInt of int
    | DStr of string
    | DBool of bool
    | DFloat of float
    | DLambda of efun
    | DUnbound
and mvalue =
    | MInt of int
    | MStr of string
    | MBool of bool
    | MFloat of float
    | MUnbound

let d_to_e = function
    | DLoc(_) -> failwith "memory addresses are not correct expression values"
    | DInt(x) -> EInt(x)
    | DStr(x) -> EStr(x)
    | DBool(x) -> EBool(x)
    | DFloat(x) -> EFloat(x)
    | DLambda(f) -> ELambda(f)
    | DUnbound -> EUnbound
let 

type env = ide -> dvalue
type store = loc -> mvalue

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
    | Val of ide (*get me the value of the memory loc this environment variable is holding*)

    (* FUNCTIONS *)
    | Plus of expr*expr
    | Multiply of expr*expr
    | Apply of expr*expr
    | Equals of expr*expr
    | Greater of expr*expr
    | Not of expr
    | Or of expr*expr
    | And of expr*expr

type com =
    (* SIDE EFFECT *)
    | Assign of ide*expr

    (* CONTROL FLOW *)
    | While of expr*com
    | CIfThen of expr*com (* more stateful to have com list? or does CSeq take care?*)
    | CIfThenElse of expr*com*com
    | CSeq of com*com

type dec = None
