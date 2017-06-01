#use "interpreter.ml";;
#load "str.cma";; (*regex Str module*)

(* NOTES:
    * reflection must check last tl == ""
    *)

let teste =
    let e1 = "Plus(Int(1), Int(2))"
    and e2 = "Plus(Int(3), Int(4))"
    in
    "Plus(" ^ e1 ^ "," ^ e2 ^ ")"

let testc =
    let e = "Equals(Bool(false),Bool(false))"
    and c1 = "Assign(\"x\",Int(3))"
    and c2 = "Assign(\"y\",Int(3))"
    in
    "CIfThenElse(" ^ e ^ "," ^ c1 ^ "," ^ c2 ^ ")"


let next_unit s =
    let re = Str.regexp "[(),]+"
    in
    match Str.bounded_split_delim re s 2 with
    | fst::snd::[] -> (String.trim fst),snd
    | _ -> failwith "next_unit error"


let rec ereflect (s:string) :expr*string =
    let hd,tl = next_unit s
    in
    match hd with
    | "Int" ->
            let i,tl = next_unit tl
            in Int(int_of_string i), tl
    | "Str" ->
            let s,tl = next_unit tl
            in Str(s),tl
    | "Bool" ->
            let b,tl = next_unit tl
            in Bool(bool_of_string b), tl
    | "Float" ->
            let f,tl = next_unit tl
            in Float(float_of_string f), tl
    (* end of special types *)
    | "Lambda" ->
            let x,tl = next_unit tl
            in let e,tl = ereflect tl
            in Lambda(x,e), tl
    | "IfThenElse" ->
            let e1, tl = ereflect tl
            in let e2, tl = ereflect tl
            in let e3, tl = ereflect tl
            in IfThenElse(e1,e2,e3), tl
    | "Var" ->
            let x,tl = next_unit tl
            in Var(x),tl
    | "LetIn" ->
            let x,tl = next_unit tl
            in let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in LetIn(x,e1,e2), tl
    | "Val" ->
            let x,tl = next_unit tl
            in Val(x),tl
    | "Plus" ->
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in Plus(e1,e2),tl
    | "Multiply" ->
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in Multiply(e1,e2),tl
    | "Apply" ->
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in Apply(e1,e2),tl
    | "Equals" ->
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in Equals(e1,e2),tl
    | "Greater" ->
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in Greater(e1,e2),tl
    | "Not" ->
            let e,tl = ereflect tl
            in Not(e),tl
    | "Or" ->
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in Or(e1,e2),tl
    | "And" -> 
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in And(e1,e2),tl
    | "Len" ->
            let e,tl = ereflect tl
            in Len(e),tl
    | "Sub" ->
            let e1,tl = ereflect tl
            in let e2,tl = ereflect tl
            in let e3,tl = ereflect tl
            in Sub(e1,e2,e3),tl
    | err -> failwith ("ereflect error: '"^err^"' is not an expression")


let rec creflect (s:string) :com*string = 
    let hd,tl = next_unit s
    in
    match hd with
    | "Assign" ->
            let x,tl = next_unit tl
            in let e,tl = ereflect tl
            in Assign(x,e),tl
    | "While" ->
            let e,tl = ereflect tl
            in let c,tl = creflect tl
            in While(e,c),tl
    | "CIfThen" -> 
            let e,tl = ereflect tl
            in let c,tl = creflect tl
            in CIfThen(e,c),tl
    | "CIfThenElse" ->
            let e,tl = ereflect tl
            in let c1,tl = creflect tl
            in let c2,tl = creflect tl
            in CIfThenElse(e,c1,c2),tl
    | "CSeq" ->
            let c1,tl = creflect tl
            in let c2,tl = creflect tl
            in CSeq(c1,c2),tl
    | err -> failwith ("creflect error: '"^err^"' is not a command")


let rec dreflect (s:string) :dec*string =
    let hd,tl = next_unit s
    in
    match hd with
    | "New" ->
            let x,tl = next_unit tl
            in let e,tl = ereflect tl
            in New(x,e),tl
    | "DSeq" ->
            let d1,tl = dreflect tl
            in let d2,tl = dreflect tl
            in DSeq(d1,d2),tl
    | err -> failwith ("dreflect error: '"^err^"' is not a declaraction")
