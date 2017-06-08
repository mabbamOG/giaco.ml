(** SEMANTIC **)
type efun = (dvalue->store->evalue)
and cproc = (dvalue list->store->store) (* needed to parametrize cval *)
and evalue =
    | EInt of int
    | EStr of string
    | EBool of bool
    | EFloat of float
    | ELambda of efun
    | EProc of cproc
    | EUnbound
and dvalue =
    | DLoc of loc
    | DInt of int
    | DStr of string
    | DBool of bool
    | DFloat of float
    | DLambda of efun
    | DProc of cproc
    | DUnbound
and mvalue =
    | MInt of int
    | MStr of string
    | MBool of bool
    | MFloat of float
    | MProc of cproc
    | MUnbound
and env = ide -> dvalue
and store = loc -> mvalue


(* utility *)
let extend_fun_map key value oldmap = function id -> if key=id then value else (oldmap id)
let env': ide->dvalue->env->env = extend_fun_map
and store': loc->mvalue->store->store = extend_fun_map

let store_size = 100
let newloc' = newloc store_size

(* utili per la transizione tra domini semantici *)
let m_to_d = function
    | MInt(x) -> DInt(x)
    | MStr(x) -> DStr(x)
    | MBool(x) -> DBool(x)
    | MFloat(x) -> DFloat(x)
    | MUnbound -> DUnbound
    | MProc(x) -> DProc(x)
and d_to_e = function
    | DInt(x) -> EInt(x)
    | DStr(x) -> EStr(x)
    | DBool(x) -> EBool(x)
    | DFloat(x) -> EFloat(x)
    | DLambda(f) -> ELambda(f)
    | DUnbound -> EUnbound
    | DLoc(_) -> failwith "memory addresses are not correct expression values"
    | DProc(x) -> EProc(x)
and e_to_m = function
    | EInt(x) -> MInt(x)
    | EStr(x) -> MStr(x)
    | EBool(x) -> MBool(x)
    | EFloat(x) -> MFloat(x)
    | EUnbound -> MUnbound
    | ELambda(_) -> failwith "lambdas are not correct modifiable memory"
    | EProc(x) -> MProc(x)
let e_to_d = function x -> m_to_d (e_to_m x)
and m_to_e = function x -> d_to_e (m_to_d x)
let d_to_m = function x -> e_to_m (d_to_e x)

let new' (x:ide) (v:dvalue) (p:env) (o:store) :env*store= let l = newloc' o in (env' x (DLoc(l)) p),(store' l (d_to_m v) o)
let rec new'' (xl:ide list) (vl:dvalue list) (p:env) (o:store) :env*store = match xl,vl with 
    | [],[] -> p,o 
    | (x::xl),(v::vl) -> let p',o' = new' x v p o in new'' xl vl p' o'
    | _ -> failwith "parameter count mismatch"
