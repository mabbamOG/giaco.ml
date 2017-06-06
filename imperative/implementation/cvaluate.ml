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

    (* COM CONTROL FLOW *)
    | CIfThenElse(b,c1,c2) -> cval (lazy_cifthenelse (eval b p o) c1 c2) p o
    | CIfThen(b,c) -> cval (CIfThenElse(b,c,Skip)) p o
    | (While(b,c)) as w -> cval (CIfThen(b,CSeq(c,w))) p o
    | CSeq(c1,c2) -> cval c2 p (cval c1 p o)
    | Skip -> o
    | Reflect(s) -> let c,other = creflect s in
                    if other == "" then cval c p o else failwith "command incomplete"

