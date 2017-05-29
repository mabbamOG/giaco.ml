(* IF *)
let _ = IfThenElse(Greater(Str("bob"),Str("mouse")),Str("ciao mondo"),Int(5))
let _ = IfThenElse(Not(Greater(Str("bob"),Str("mouse"))),Str("ciao mondo"),Int(5))

(* LETs *)
let _ =
    let xxx = LetIn("a",Int(3),Multiply(Var("a"),Var("a")))
    in
LetIn("a",Int(5),(LetIn("b",xxx,LetIn("c",Int(6),Plus(Var("a"),Plus(Var("b"),Var("c")))))))
