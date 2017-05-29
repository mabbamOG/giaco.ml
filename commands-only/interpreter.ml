#use "domains.ml";;
#use "implementation/evaluate.ml";;
#use "implementation/cvaluate.ml";;

let store_size = 100
let newloc' = newloc store_size

let emptyenv:env = function "" -> DUnbound | other -> failwith ("'"^other^"' not in environment") (*utility*)
type storemem = mvalue array
let emptystore:store = function key -> Array.get ([|MUnbound|]:storemem) (Nativeint.to_int key)
