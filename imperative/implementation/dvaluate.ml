let rec dval (d:dec) (p:env) (o:store) :env*store =
    match d with
    | New(x,e) ->  let l = newloc' o in (env' x (DLoc(l)) p),( store' l (e_to_m (eval e p o)) o)
    | DSeq(d1,d2) -> let p',o' = (dval d1 p o) in dval d2 p' o'
