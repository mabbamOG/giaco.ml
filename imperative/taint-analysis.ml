(* get tainting domains *)
#use "tainting/domains.ml";;

(* get tainting implementation *)
#use "tainting/etaint.ml";;
#use "tainting/ctaint.ml";;
#use "tainting/dtaint.ml";;

(* utility *)

let show_tenv (p:tenv) = EnvMap.bindings p
let show_tstore (o:tstore) = StoreMap.bindings o

(** TAINT ANALYSIS **)
let taint_analysis (src:prog) (p:tenv) (o:tstore) =
    match src with Prog(ds,cs) ->
        let p,o = dtaint ds p o
        in let o,_ = ctaint cs p o
        in show_tenv p, show_tstore o

(** TEST CASE **)
(*let p = tenv' "xx" (Taint(Dirty)) emptytenv |> tenv' "yy" (Taint(Clean)) |> tenv' "clean" (TLoc(l))
let p,o = tnew' "clean" Clean p o
let p,o = tnew' "dirty" Dirty p o*)

(*
let e = Equals(Plus(Val("x"),Val("y")),Int(6))
let assign1 = CSeq(Assign("x", Val("dirty")), Assign("y", Val("clean")))
let assign2 = CSeq(Assign("x", Val("clean")), Assign("y", Val("dirty")))
let if2 = CIfThenElse(e,assign2,assign1) 
let if1 = CIfThen(e,CSeq(assign1,if2))

let d = DSeq(New("x", Val("dirty")), New("y", Val("clean")))

let result = taint_analysis (Prog(d,if1)) p o
*)
