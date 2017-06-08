(** PARSING AND INTERNAL DOMAINS **)
#use "domains/generic.ml";;
#use "domains/syntax.ml";;
#use "domains/semantic.ml";;

(** REFLECTION AND TAINT-ANALYSIS **)
#use "reflect.ml";;
#use "taint-analysis";;

(** EVALUATION of EXPR/COM/DEC **)
#use "implementation/evaluate.ml";;
#use "implementation/cvaluate.ml";;
#use "implementation/dvaluate.ml";;

let interpret' (src:prog) (p:env) (o:store) =
    match src with Prog(ds,cs) -> let p',o' = dval ds p o in p', cval cs p' o'
let interpret (src:prog) (p:env) (o:store) = let _,o' = interpret' src p o in o'


(* utility *)
let emptyenv:env = function "" -> DUnbound | other -> failwith ("'"^other^"' not in environment") 
type storemem = mvalue array
let emptystore:store = function key -> Array.get ([|MUnbound|]:storemem) (Nativeint.to_int key)
