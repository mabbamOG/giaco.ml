let rec ctaint (c:com) (p:tenv) (o:tstore) :tstore*tstore =
    let or_reduce (o1:tstore) (o2:tstore) =
        let f k t1 t2 = Some(tor t1 t2)
        in
        StoreMap.union f o1 o2
    and dirty_merge (o1:tstore) (o2:tstore) =
        let o = tstore'' o1 o2
        in
        StoreMap.map (function t -> Dirty) o
    in
    match c with
    | Assign(x,e) -> 
            let l,t = match (tenv_get x p) with
                | TLoc(l) -> l, (etaint e p o)
                | _ -> failwith ("tassign error "^x)
            in (tstore' l t o),(tstore' l t emptytstore)
    | CIfThenElse(e,c1,c2) -> 
            let (_,o1),(_,o2) = (ctaint c1 p o),(ctaint c2 p o)
            in let o3 =
                if (etaint e p o)=Dirty
                then or_reduce o1 o2
                else dirty_merge o1 o2
            in (tstore'' o3 o), o3
    | CIfThen(e,c) -> ctaint (CIfThenElse(e,c,Skip)) p o
    | While(e,c) -> ctaint (CIfThen(e,c)) p o
    | CSeq(c1,c2) ->
            let o, o' = ctaint c1 p o
            in let o, o'' = ctaint c2 p o
            in o, (tstore'' o' o'')
    | Reflect(s) ->
            let c,_ = creflect s
            in ctaint c p o
    | Skip -> o, emptytstore
