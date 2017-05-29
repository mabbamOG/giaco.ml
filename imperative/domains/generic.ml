(** GENERIC **)
type ide = string
type loc = nativeint


(* utility *)
let rec newloc (max_store_size:int) o :loc = 
    let l = Random.nativeint (Nativeint.of_int max_store_size)
    in
    try (let _ = (o l) in newloc max_store_size o)
    with _ -> l
