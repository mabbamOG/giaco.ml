type taint = Dirty | Clean
type tdvalue = Taint of taint | TLoc of loc
module EnvMap = Map.Make(struct type t = ide let compare = Pervasives.compare end)
type tenv = tdvalue EnvMap.t
module StoreMap = Map.Make(struct type t = loc let compare = Pervasives.compare end)
type tstore = taint StoreMap.t

(* utilities *)
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
let tor t1 t2 = if t1=t2 then t1 else Dirty
let tor3 t1 t2 t3 = tor (tor t1 t2) t3

(* generic *)
let rec tnewloc (max_store_size:int) o :loc =
    let l = Random.nativeint (Nativeint.of_int max_store_size)
    in
    try (let _ = (tstore_get l) in tnewloc max_store_size o)
    with _ -> l

let store_size = 100
let tnewloc' = tnewloc store_size

let emptytenv:tenv = EnvMap.empty
let emptytstore:tstore = StoreMap.empty
