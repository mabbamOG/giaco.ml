(* ISSUES:
    * some ops must be non-strict -> and/if/or
    *)
let rec eval (e:expr) (p:env) (o:store) :evalue = 
    let 
    plus v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EInt(x+y) 
        | EFloat(x),EFloat(y) -> EFloat(x+.y) 
        | EStr(x),EStr(y) -> EStr(x^y)
        | _ -> failwith "plus error"
    and
    multiply v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EInt(x*y) 
        | EFloat(x),EFloat(y) -> EFloat(x*.y) 
        | _ -> failwith "multiply error"
    and
    apply v1 v2 o = match v1 with
        | ELambda(f) -> f (e_to_d v2) o
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
        | EUnbound,EUnbound -> EBool(false)
        | _ -> failwith "equals error"
    and
    _not = function
        | EBool(b) -> EBool(not b)
        | _ -> failwith "not error"
    and
    lazy_or (b:evalue) (e:expr) = match b with
        | EBool(true) -> Bool(true)
        | EBool(false) -> e
        | _ -> failwith "lazy or error"
    and
    lazy_and (b:evalue) (e:expr) = match b with
        | EBool(false) -> Bool(false)
        | EBool(true) -> e
        | _ -> failwith "lazy and error"
    and
    (* only function not value->value->value *)
    lazy_ifthenelse (b:evalue) (e1:expr) (e2:expr) =
        match b with
        | EBool(true) -> e1
        | EBool(false) -> e2
        | _ -> failwith "ifthenelse error"
    and
    _val (x:dvalue) (o':store) = match x with
        | DLoc(l) -> m_to_e (o' l)
        | _ -> failwith "_val error"
    in
    match e with
    (* EXPR TYPES *)
    | Int(i) -> EInt(i)
    | Str(s) -> EStr(s)
    | Bool(b) -> EBool(b)
    | Float(f) -> EFloat(f)
    | Lambda(x,e) -> ELambda(function v -> function o' -> eval e (env' x v p) o')

    (* EXPR CONTROL FLOW *)
    | IfThenElse(b,e1,e2) -> eval (lazy_ifthenelse (eval b p o) e1 e2) p o

    (* EXPR DEREFERENCE *)
    | Var(x) -> d_to_e (p x)
    | LetIn(x,e1,e2) -> eval e2 (env' x (e_to_d (eval e1 p o)) p) o
    | Val(x) -> _val (p x) o

    (* EXPR FUNCTIONS *)
    | Plus(e1,e2) -> plus (eval e1 p o) (eval e2 p o)
    | Multiply(e1,e2) -> multiply (eval e1 p o) (eval e2 p o)
    | Apply(e1,e2) -> apply (eval e1 p o) (eval e2 p o) o
    | Greater(e1,e2) -> greater_than (eval e1 p o) (eval e2 p o)
    | Equals(e1,e2) -> equals (eval e1 p o) (eval e2 p o)
    | Not(e1) -> _not (eval e1 p o)
    | Or(e1,e2) -> eval (lazy_or (eval e1 p o) e2) p o
    | And(e1,e2) -> eval (lazy_and (eval e1 p o) e2) p o
