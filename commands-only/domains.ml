(** GENERIC **)
type ide = string
type loc = nativeint
let newloc: int -> loc = function max_store_size -> Random.nativeint (Nativeint.of_int max_store_size)
;;#use "domains/semantics.ml";;
#use "domains/syntax.ml";;
