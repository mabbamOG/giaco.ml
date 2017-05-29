(** PARSING AND INTERNAL DOMAINS **)
#use "domains/generic.ml";;
#use "domains/semantic.ml";;
#use "domains/syntax.ml";;


(** EVALUATION of EXPR/COM/DEC **)
#use "implementation/evaluate.ml";;
#use "implementation/cvaluate.ml";;
#use "implementation/dvaluate.ml";;
let interpret (src:prog) (p:env) (o:store) =
    match src with Prog(ds,cs) -> let p',o' = dval ds p o in cval cs p' o'


(* utility *)
let emptyenv:env = function "" -> DUnbound | other -> failwith ("'"^other^"' not in environment") 
type storemem = mvalue array
let emptystore:store = function key -> Array.get ([|MUnbound|]:storemem) (Nativeint.to_int key)