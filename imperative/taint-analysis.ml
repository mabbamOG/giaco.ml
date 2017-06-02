#use "interpreter.ml";;
type taint = Dirty | Clean
type tdvalue = Taint of taint | TLoc of loc
module EnvMap = Map.Make(struct type t = ide let compare = Pervasives.compare end)
type tenv = tdvalue EnvMap.t
module StoreMap = Map.Make(struct type t = loc let compare = Pervasives.compare end)
type tstore = taint StoreMap.t
let tenv_get key p = EnvMap.find key p
let tstore_get key o = StoreMap.find key o
let td_to_t = function
    | Taint(t) -> t
    | TLoc(l) -> failwith ("memory address not correct taint value"^(Nativeint.to_string l))
and t_to_td = function t -> Taint(t)
let tenv' (k:ide) (v:tdvalue) (p:tenv) :tenv =  EnvMap.add k v p
let tstore' (k:loc) (v:taint) (o:tstore) :tstore = StoreMap.add k v o
let tstore'' (o:tstore) (oldstore:tstore) :tstore =
    let f k t oldt = 
        match t with 
        | Some(t) -> Some(t)
        | None -> (match oldt with Some(t) -> Some(t) | None -> failwith "tstore'' error")
    in
    StoreMap.merge f o oldstore

let emptytenv:tenv = EnvMap.empty
let emptytstore:tstore = StoreMap.empty

(*let taint_analysis: Prog -> tenv -> tstore -> (tenv*tstore)*)

let tor t1 t2 = if t1=t2 then t1 else Dirty
let tor3 t1 t2 t3 = tor (tor t1 t2) t3

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
    | Skip -> o, emptytstore

let rec tnewloc (max_store_size:int) o :loc =
    let l = Random.nativeint (Nativeint.of_int max_store_size)
    in
    try (let _ = (tstore_get l) in tnewloc max_store_size o)
    with _ -> l

let store_size = 100
let tnewloc' = tnewloc store_size

let rec dtaint (d:dec) (p:tenv) (o:tstore) :(tenv*tstore) =
    match d with
    | New(x,e) -> 
            let l = (tnewloc' o) 
            in (tenv' x (TLoc(l)) p), (tstore' l (etaint e p o) o)
    | DSeq(d1,d2) -> 
            let p,o = dtaint d1 p o
            in dtaint d2 p o

let show_tenv (p:tenv) = EnvMap.bindings p
let show_tstore (o:tstore) = StoreMap.bindings o

let taint_analysis (src:prog) (p:tenv) (o:tstore) =
    match src with Prog(ds,cs) ->
        let p,o = dtaint ds p o
        in let o,_ = ctaint cs p o
        in show_tenv p, show_tstore o


        (* TEST CASE *)
let o = emptytstore
let l = tnewloc' o
let p = tenv' "xx" (Taint(Dirty)) emptytenv |> tenv' "yy" (Taint(Clean)) |> tenv' "clean" (TLoc(l))
let o = tstore' l Clean o
let l = tnewloc' o
let o = tstore' l Dirty o
let p = tenv' "dirty" (TLoc(l)) p

let e = Equals(Plus(Val("x"),Val("y")),Int(6))
let assign1 = CSeq(Assign("x", Val("dirty")), Assign("y", Val("clean")))
let assign2 = CSeq(Assign("x", Val("clean")), Assign("y", Val("dirty")))
let if2 = CIfThenElse(e,assign2,assign1) 
let if1 = CIfThen(e,CSeq(assign1,if2))

let d = DSeq(New("x", Val("dirty")), New("y", Val("clean")))

let result = taint_analysis (Prog(d,if1)) p o
