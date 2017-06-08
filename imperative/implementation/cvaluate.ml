let dval_ref = ref (fun _->assert false)
let rec cval (c:com) (p:env) (o:store) :store= 
    let assign (x:dvalue) (e:evalue) (o':store) = match x with
        | DLoc(l) -> store' l (e_to_m e) o'
        | _ -> failwith "assign error"
    and lazy_cifthenelse (b:evalue) (c1:com) (c2:com) = match b with
        | EBool(true) -> c1
        | EBool(false) -> c2
        | _ -> failwith "imperative if then else error"
    in
    match c with
    (* COM SIDE EFFECT *)
    | Assign(x,e) -> assign (p x) (eval e p o) o

    (* BLOCKS AND SUBPROCESSES *)
    | Block(d,c) -> let p',o' = !dval_ref d p o in cval c p' o'
    | Call(e,xe) -> (match eval e p o with EProc(f) -> f (List.map (fun e->e_to_d (eval e p o)) xe) o | _-> failwith "not a valid procedure call")
    (* COM CONTROL FLOW *)
    | CIfThenElse(b,c1,c2) -> cval (lazy_cifthenelse (eval b p o) c1 c2) p o
    | CIfThen(b,c) -> cval (CIfThenElse(b,c,CSkip)) p o
    | (While(b,c)) as w -> cval (CIfThen(b,CSeq(c,w))) p o
    | CSeq(c1,c2) -> cval c2 p (cval c1 p o)
    | CSkip -> o
    | Reflect(e) -> (match e with Str(s) -> let c,tl = creflect s in if tl == "" then cval c p o else failwith "command incomplete"
                    | _ -> failwith "can only call Reflect on string")
;;cval_ref := cval (* needed to reference cval from eval *)
