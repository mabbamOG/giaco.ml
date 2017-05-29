#use "interpreter.ml";;
let checke a b p o = eval (Equals(a,b)) p o
;;print_endline "--------------------------------------------------------TESTING:"
;;print_endline "------------expressions-----------------------------------------"
(* IF *)
let _ = checke (Int(5)) (IfThenElse(Greater(Str("bob"),Str("mouse")),Str("ciao mondo"),Int(5))) emptyenv emptystore
let _ = checke  (Str("ciao mondo")) (IfThenElse(Not(Greater(Str("bob"),Str("mouse"))),Str("ciao mondo"),Int(5))) emptyenv emptystore

(* LETs *)
let _ = 
    let xxx = LetIn("a",Int(3),Multiply(Var("a"),Var("a")))
    in
checke (Int(20)) (LetIn("a",Int(5),(LetIn("b",xxx,LetIn("c",Int(6),Plus(Var("a"),Plus(Var("b"),Var("c")))))))) emptyenv emptystore
;;print_endline "------------commands--------------------------------------------"
let checkenv = (env' "x" (DLoc(Nativeint.of_int 3)) emptyenv)
let checkstore = (store' (Nativeint.of_int 3) (MInt(0)) emptystore)
let checkc c = cval c  checkenv checkstore
let _ = checke (Val("x")) (Int(13)) checkenv (checkc (Assign("x",Plus(Int(5),Int(8)))))
let _ = 
    let c1 = CIfThenElse(Equals(Str("aaaa"),Str("bbbb")),Assign("x",Str("a")),Assign("x",Str("b")))
    and c2 = CIfThen(Equals(Val("x"),Str("a")),Assign("x",Bool(true)))
    in
    let xxx = CSeq(c1,c2)
    in
    checke (Val("x")) (Str("b"))  checkenv (checkc xxx)
let _ = 
    let c1 = CIfThenElse(Not(Equals(Str("aaaa"),Str("bbbb"))),Assign("x",Str("a")),Assign("x",Str("b")))
    and c2 = CIfThen(Equals(Val("x"),Str("a")),Assign("x",Bool(true)))
    in
    let xxx = CSeq(c1,c2)
    in
    checke (Val("x")) (Bool(true))  checkenv (checkc xxx)
let _ = checke (Val("x")) (Int(0)) checkenv (checkc (CSeq(Skip,Skip)))
