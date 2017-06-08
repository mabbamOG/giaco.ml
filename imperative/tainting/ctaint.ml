let dtaint_ref = ref (fun _->assert false)
let rec ctaint (c:com) (p:tenv) (o:tstore) :tstore*tstore =
    let or_reduce (o1:tstore) (o2:tstore) =
        (* all same values must be or'ed *)
        let f k t1 t2 = Some(tor t1 t2)
        in
        StoreMap.union f o1 o2
    and dirty_merge (o1:tstore) (o2:tstore) =
        (* all same values are dirty *)
        let f k t1 t2 = Some(Dirty)
        in
        StoreMap.union f o1 o2
    in
    match c with
    | Assign(x,e) -> 
            let l,t = match (tenv_get x p) with
                | TLoc(l) -> l, (etaint e p o)
                | _ -> failwith ("tassign error "^x)
            in (tstore' l t o),(tstore' l t emptytstore)
    | Block(d,c) -> let p',o' = !dtaint_ref d p o in ctaint c p' o'
    | Call(e,el) -> failwith "call not taintable"
    | CIfThenElse(e,c1,c2) -> 
            let (_,o1),(_,o2) = (ctaint c1 p o),(ctaint c2 p o)
            in let o3 =
                if (etaint e p o)=Dirty
                then or_reduce o1 o2
                else dirty_merge o1 o2
            in (tstore'' o3 o), o3
    | CIfThen(e,c) -> ctaint (CIfThenElse(e,c,CSkip)) p o
    | While(e,c) -> ctaint (CIfThen(e,c)) p o
    | CSeq(c1,c2) ->
            let o, o' = ctaint c1 p o
            in let o, o'' = ctaint c2 p o
            in o, (tstore'' o' o'')
    | Reflect(e) -> (match etaint e p o with
                    | Dirty -> failwith "warning: reflect attack!"
                    | Clean -> o),emptytstore
    | CSkip -> o, emptytstore
