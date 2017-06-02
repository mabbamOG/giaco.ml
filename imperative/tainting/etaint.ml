let rec etaint (e:expr) (p:tenv) (o:tstore) :taint =
    let lazy_tor (e1:expr) (e2:expr) (p:tenv) (o:tstore) :taint = if (etaint e1 p o)=Dirty then Dirty else (etaint e2 p o)
    in let lazy_tor3 e1 e2 e3 p o = if (lazy_tor e1 e2 p o)=Dirty then Dirty else etaint e3 p o
    in
    match e with
    | Int(_)|Str(_)|Bool(_)|Float(_) -> 
            Clean
    | Var(x) -> td_to_t (tenv_get x p)
    | Val(x) -> (match (tenv_get x p) with TLoc(l) -> (tstore_get l o) | _ -> failwith "not a pointer")
    | IfThenElse(e1,e2,e3) -> lazy_tor3 e1 e2 e3 p o
    | LetIn(x,e1,e2) -> etaint e2 (tenv' x (t_to_td(etaint e1 p o)) p) o
    | Lambda(x,e) -> let p' = tenv' x (t_to_td(Clean)) p in tor (etaint e p' o) Clean
    | Apply(e1,e2) -> lazy_tor e1 e2 p o
    
    | Plus(e1,e2)|Multiply(e1,e2)|Greater(e1,e2)|Equals(e1,e2)|Or(e1,e2)|And(e1,e2) -> 
            lazy_tor e1 e2 p o
    | Not(e)|Len(e) -> etaint e p o
    | Sub(e1,e2,e3) -> lazy_tor3 e1 e2 e3 p o
