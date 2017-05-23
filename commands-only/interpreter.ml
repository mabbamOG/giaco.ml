#use "domains.ml";;
let env': ide->value->env->env = extend_fun_map
let store': loc->mvalue->store->store = extend_fun_map

let store_size = 100
let newloc' = newloc store_size
(* ISSUES:
    * match tuples instead of function e1 e2
    * look at equals with all that duplication!
    * some ops must be non-strict -> and/if/or
    *)
let rec eval (e:expr) (p:env) (o:store) :value = 
    let 
    plus v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EInt(x+y) 
        | EFloat(x),EFloat(y) -> EFloat(x+.y) 
        | _ -> failwith "plus error"
    and
    multiply v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EInt(x*y) 
        | EFloat(x),EFloat(y) -> EFloat(x*.y) 
        | _ -> failwith "multiply error"
    and
    apply v1 v2 = match v1 with
        | ELambda(f) -> f v2
        | _ -> failwith "apply error"
    and
    greater_than v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EBool(x>y)
        | EFloat(x),EFloat(y) -> EBool(x>y)
        | EStr(x),EStr(y) -> EBool((String.length x)>(String.length y))
        | _ -> failwith "greater_than error"
    and
    equals v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EBool(x=y)
        | EFloat(x),EFloat(y) -> EBool(x=y)
        | EStr(x),EStr(y) -> EBool(x=y)
        | EBool(x),EBool(y) -> EBool(x=y)
        | Unbound,Unbound -> EBool(false)
        | _ -> failwith "equals error"
    and
    _not = function
        | EBool(b) -> EBool(not b)
        | _ -> failwith "not error"
    and
    _or v1 v2 = match v1,v2 with
        | EBool(a),EBool(b) -> EBool(a||b)
        | _ -> failwith "or error"
    and
    _and v1 v2 = match v1,v2 with
        | EBool(a),EBool(b) -> EBool(a&&b)
        | _ -> failwith "and error"
    and
    (* only function not value->value->value *)
    lazy_ifthenelse (b:value) (e1:expr) (e2:expr) =
        match b with
        | EBool(true) -> e1
        | EBool(false) -> e2
        | _ -> failwith "ifthenelse error"
    in
    match e with
    (* EXPR TYPES *)
    | Int(i) -> EInt(i)
    | Str(s) -> EStr(s)
    | Bool(b) -> EBool(b)
    | Float(f) -> EFloat(f)
    | Lambda(x,e) -> ELambda(function v -> eval e (env' x v p) o)

    (* EXPR CONTROL FLOW *)
    | IfThenElse(b,e1,e2) -> eval (lazy_ifthenelse (eval b p o) e1 e2) p o

    (* EXPR DEREFERENCE *)
    | Var(x) -> d_to_e (p x)
    | LetIn(x,e1,e2) -> eval e2 (env' x (eval e1 p) p) o
    | Val(x) -> m_to_e match....(p x)

    (* EXPR FUNCTIONS *)
    | Plus(e1,e2) -> plus (eval e1 p) (eval e2 p)
    | Multiply(e1,e2) -> multiply (eval e1 p) (eval e2 p)
    | Apply(e1,e2) -> apply (eval e1 p) (eval e2 p)
    | Greater(e1,e2) -> greater_than (eval e1 p) (eval e2 p)
    | Equals(e1,e2) -> equals (eval e1 p) (eval e2 p)
    | Not(e1) -> _not (eval e1 p)
    | Or(e1,e2) -> _or (eval e1 p) (eval e2 p)
    | And(e1,e2) -> _and (eval e1 p) (eval e2 p)

let rec cval (c:com) (p:env) (o:store) = match c with
    (* COM SIDE EFFECT *)
    | Assign(x,e) -> match (p x) with DLoc(l) -> store' l (eval e p o) | _-> failwith "this is not a memory pointer!"

    (* COM CONTROL FLOW *)
    | While(b,c)
    | CIfThen(b,c)
    | CIfThenElse(b,c1,c2)

let emptyenv:env = function "" -> Unbound | other -> failwith ("'"^other^"' not in environment") (*utility*)
type storemem = mvalue array
let emptystore:store = function key -> Array.get ([|MUnbound|]:storemem) (Nativeint.to_int key)
