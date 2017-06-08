let rec dtaint (d:dec) (p:tenv) (o:tstore) :(tenv*tstore) =
    match d with
    | New(x,e) -> 
            let l = (tnewloc' o) 
            in (tenv' x (TLoc(l)) p), (tstore' l (etaint e p o) o)
    | DSeq(d1,d2) -> 
            let p,o = dtaint d1 p o
            in dtaint d2 p o
    | DSkip -> p,o
;;dtaint_ref:=dtaint
