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
and env = ide -> dvalue
and store = loc -> mvalue


(* utility *)
let extend_fun_map key value oldmap = function id -> if key=id then value else (oldmap id)
and env': ide->dvalue->env->env = extend_fun_map
and store': loc->mvalue->store->store = extend_fun_map

let store_size = 100
and newloc' = newloc store_size

let m_to_d = function
    | MInt(x) -> DInt(x)
    | MStr(x) -> DStr(x)
    | MBool(x) -> DBool(x)
    | MFloat(x) -> DFloat(x)
    | MUnbound -> DUnbound
and d_to_e = function
    | DInt(x) -> EInt(x)
    | DStr(x) -> EStr(x)
    | DBool(x) -> EBool(x)
    | DFloat(x) -> EFloat(x)
    | DLambda(f) -> ELambda(f)
    | DUnbound -> EUnbound
    | DLoc(_) -> failwith "memory addresses are not correct expression values"
and e_to_m = function
    | EInt(x) -> MInt(x)
    | EStr(x) -> MStr(x)
    | EBool(x) -> MBool(x)
    | EFloat(x) -> MFloat(x)
    | EUnbound -> MUnbound
    | ELambda(_) -> failwith "lambdas are not correct modifiable memory"

let e_to_d = function x -> m_to_d (e_to_m x)
and m_to_e = function x -> d_to_e (m_to_d x)
