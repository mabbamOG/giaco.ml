#use "domains.ml";;
let env' (key:ide) (value:value) (oldenv:env): env = 
    function id -> if id=key then value else (oldenv id)

(* ISSUES:
    * match tuples instead of function e1 e2
    * look at equals with all that duplication!
    * some ops must be non-strict -> and/if/or
    *)
let rec eval (e:expr) (p:env) :value = 
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
    lazy_ifthenelse (c:value) (e1:expr) (e2:expr) =
        match c with
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
    | Lambda(x,e) -> ELambda(function v -> eval e (env' x v p))

    (* EXPR CONTROL FLOW *)
    | IfThenElse(c,e1,e2) -> eval (lazy_ifthenelse (eval c p) e1 e2) p

    (* EXPR DEREFERENCE *)
    | Var(x) -> (p x)
    | LetIn(x,e1,e2) -> eval e2 (env' x (eval e1 p) p)

    (* EXPR FUNCTIONS *)
    | Plus(e1,e2) -> plus (eval e1 p) (eval e2 p)
    | Multiply(e1,e2) -> multiply (eval e1 p) (eval e2 p)
    | Apply(e1,e2) -> apply (eval e1 p) (eval e2 p)
    | Greater(e1,e2) -> greater_than (eval e1 p) (eval e2 p)
    | Equals(e1,e2) -> equals (eval e1 p) (eval e2 p)
    | Not(e1) -> _not (eval e1 p)
    | Or(e1,e2) -> _or (eval e1 p) (eval e2 p)
    | And(e1,e2) -> _and (eval e1 p) (eval e2 p)

let emptyenv:env = function "" -> Unbound | other -> failwith ("'"^other^"' not in environment") (*utility*)
