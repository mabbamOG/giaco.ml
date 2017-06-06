#use "interpreter.ml";;
let checke a b p o = eval (Equals(a,b)) p o
let checke' a b = checke a b emptyenv emptystore
let checkenv = (env' "x" (DLoc(Nativeint.of_int 3)) emptyenv)
let checkstore = (store' (Nativeint.of_int 3) (MInt(0)) emptystore)
let checkc c = cval c checkenv checkstore
let checkd d = dval d checkenv checkstore
;;print_endline "--------------------------------------------------------TESTING:"


;;print_endline "------------expressions-----------------------------------------"
(* IF *)
let _ = checke' (Int(5)) (IfThenElse(Greater(Str("bob"),Str("mouse")),Str("ciao mondo"),Int(5)))
let _ = checke'  (Str("ciao mondo")) (IfThenElse(Not(Greater(Str("bob"),Str("mouse"))),Str("ciao mondo"),Int(5)))

(* LETs *)
let _ = 
    let xxx = LetIn("a",Int(3),Multiply(Var("a"),Var("a")))
    in
checke' (Int(20)) (LetIn("a",Int(5),(LetIn("b",xxx,LetIn("c",Int(6),Plus(Var("a"),Plus(Var("b"),Var("c"))))))))
let _ = checke' (Bool(false)) (And(Equals(Float(4.5),Float(4.6)),Equals(Float(0.1),Float(0.1))))
let _ = checke' (Bool(true)) (Or(Equals(Float(4.5),Float(4.6)),Equals(Float(0.1),Float(0.1))))


;;print_endline "------------commands--------------------------------------------"
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

let _ =
    let p  = env' "y" (DLoc(Nativeint.of_int 10)) (env' "x" (DLoc(Nativeint.of_int 9)) emptyenv)
    and o = store' (Nativeint.of_int 10) (MInt(0)) (store' (Nativeint.of_int 9) (MInt(0)) emptystore)
    in
    let o' =
    let c = While(Not(Equals(Val("x"),Int(10))),
    CSeq (Assign("y", Plus(Val("y"),Int(5))),
    Assign("x", Plus(Val("x"),Int(1)))
    )) 
    in
    cval c p o
    in

    let e1 = Equals(Val("x"),Int(10))
    and e2 = Equals(Val("y"),Int(50))
    in
    eval (And(e1,e2)) p o', eval (Val("x")) p o', eval (Val("y")) p o'


;;print_endline "------------declarations----------------------------------------"
let _ = 
    let p,o = checkd (New("a",Plus(Str("hello"),Str(" world!"))))
    in
    checke (Val("a")) (Str("hello world!")) p o

let _ =
    let d = DSeq(New("x",Int(0)),New("y",Int(0)))
    in
    let c = While(Not(Equals(Val("x"),Int(10))),
    CSeq (Assign("y", Plus(Val("y"),Int(5))),
    Assign("x", Plus(Val("x"),Int(1)))
    )) 
    in
    let p', o' = interpret' (Prog(d,c)) emptyenv emptystore
    in
    let e1 = Equals(Val("x"),Int(10)) in
    let e2 = Equals(Val("y"),Int(50)) in
    eval (And(e1,e2)) p' o', eval (Val("x")) p' o', eval (Val("y")) p' o';;
