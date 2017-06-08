let cval_ref = ref (fun _->assert false)
let rec eval (e:expr) (p:env) (o:store) :evalue = 
    let plus v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EInt(x+y) 
        | EFloat(x),EFloat(y) -> EFloat(x+.y) 
        | EStr(x),EStr(y) -> EStr(x^y)
        | _ -> failwith "plus error"
    and multiply v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EInt(x*y) 
        | EFloat(x),EFloat(y) -> EFloat(x*.y) 
        | EStr(s),EInt(n)|EInt(n),EStr(s) -> EStr(Array.fold_left (^) "" (Array.make n s))
        | _ -> failwith "multiply error"
    and apply v1 v2 o = match v1 with
        | ELambda(f) -> f (e_to_d v2) o
        | _ -> failwith "apply error"
    and greater_than v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EBool(x>y)
        | EFloat(x),EFloat(y) -> EBool(x>y)
        | EStr(x),EStr(y) -> EBool((String.length x)>(String.length y))
        | _ -> failwith "greater_than error"
    and equals v1 v2 = match v1,v2 with
        | EInt(x),EInt(y) -> EBool(x=y)
        | EFloat(x),EFloat(y) -> EBool(x=y)
        | EStr(x),EStr(y) -> EBool(x=y)
        | EBool(x),EBool(y) -> EBool(x=y)
        | EUnbound,EUnbound -> EBool(false)
        | _ -> failwith "equals error"
    and _not = function
        | EBool(b) -> EBool(not b)
        | _ -> failwith "not error"
    and lazy_or (b:evalue) (e:expr) = match b with
        | EBool(true) -> Bool(true)
        | EBool(false) -> e
        | _ -> failwith "lazy or error"
    and lazy_and (b:evalue) (e:expr) = match b with
        | EBool(false) -> Bool(false)
        | EBool(true) -> e
        | _ -> failwith "lazy and error"
    and lazy_ifthenelse (b:evalue) (e1:expr) (e2:expr) =
        match b with
        | EBool(true) -> e1
        | EBool(false) -> e2
        | _ -> failwith "ifthenelse error"
    and _val (x:dvalue) (o':store) = match x with
        | DLoc(l) -> m_to_e (o' l)
        | _ -> failwith "_val error"
    and len v = match v with
        | EStr(s) -> EInt(String.length s)
        | _ -> failwith "len error"
    and sub v1 v2 v3 = match v1,v2,v3 with
        | EStr(s),EInt(i),EInt(j) -> if i<=j then EStr(String.sub s i (j-i+1)) else EStr("")
        | _ -> failwith "sub error"
    and lower v = match v with
        | EStr(s) -> EStr(String.lowercase_ascii s)
        | _ -> failwith "lower error"
    and upper v = match v with
        | EStr(s) -> EStr(String.uppercase_ascii s)
        | _ -> failwith "upper error"
    and trim v = match v with
        | EStr(s) -> EStr(String.trim s)
        | _ -> failwith "trim error"
    and replace v1 v2 v = match v1,v2,v with
        | EStr(s1),EStr(s2),EStr(s) -> EStr(Str.global_replace (Str.regexp s1) s2 s)
        | _ -> failwith "replace error"
    in
    match e with
    (* TYPES *)
    | Int(i) -> EInt(i)
    | Str(s) -> EStr(s)
    | Bool(b) -> EBool(b)
    | Float(f) -> EFloat(f)

    (* SUBPROGRAMS *)
    | Lambda(x,e) -> ELambda(function v -> function o' -> eval e (env' x v p) o')
    | RecLambda(xf,x,e) -> 
            let rec f = function v -> function o' -> eval e (env' xf (DLambda(f)) (env' x v p)) o' in
            ELambda(f)
    | Rec(xf,e) -> (match e with
        | Lambda(x,e) -> eval (RecLambda(xf,x,e)) p o
        | _ -> failwith "only a function may be recursive...")

    (* PROCEDURE *)
    | Proc(xl,c) -> (match c with
        | Block(_) -> EProc(function vl -> function o' -> let p',o'= new'' xl vl p o' in !cval_ref c p' o')
        | _-> failwith "not a valid proc")

    (* CONTROL FLOW *)
    | IfThenElse(b,e1,e2) -> eval (lazy_ifthenelse (eval b p o) e1 e2) p o

    (* DEREFERENCE & BLOCKS *)
    | Var(x) -> d_to_e (p x)
    | LetIn(x,e1,e2) -> eval e2 (env' x (e_to_d (eval e1 p o)) p) o
    | Val(x) -> _val (p x) o

    (* FUNCTIONS *)
    | Plus(e1,e2) -> plus (eval e1 p o) (eval e2 p o)
    | Multiply(e1,e2) -> multiply (eval e1 p o) (eval e2 p o)
    | Apply(e1,e2) -> apply (eval e1 p o) (eval e2 p o) o
    | Greater(e1,e2) -> greater_than (eval e1 p o) (eval e2 p o)
    | Equals(e1,e2) -> equals (eval e1 p o) (eval e2 p o)
    | Not(e1) -> _not (eval e1 p o)
    | Or(e1,e2) -> eval (lazy_or (eval e1 p o) e2) p o
    | And(e1,e2) -> eval (lazy_and (eval e1 p o) e2) p o
    | Len(e) -> len (eval e p o)
    | Sub(e1,e2,e3) -> sub (eval e1 p o) (eval e2 p o) (eval e3 p o)
    | Lower(e) -> lower (eval e p o)
    | Upper(e) -> upper (eval e p o)
    | Trim(e) -> trim (eval e p o)
    | Replace(s1, s2, s) -> replace (eval s1 p o) (eval s2 p o) (eval s p o)
