let rec dval (d:dec) (p:env) (o:store) :env*store =
    match d with
    | New(x,e) ->  new' x (e_to_d (eval e p o)) p o
    | DSeq(d1,d2) -> let p',o' = (dval d1 p o) in dval d2 p' o'
    | DSkip -> p,o
;;dval_ref:=dval
