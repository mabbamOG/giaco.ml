(*let split s = match String.index s '(' with
| i -> String.sub s 0 i ^ " - " ^ String.sub s (i+1) (String.length s)
| exception _ -> failwith "not valid syntax"

(*let _ = split "hello" *) (* this works *)
let _ = split "Int(4)"*)

type token = Stuff of string | Comma | Bopen | Bclose
let rec tokenize tok = function
| []->[]
| hd::tl -> Stuff(hd)::tok::(tokenize tok tl)


let lookfortoken s = match String.index '(' s
| yes -> go look for ')'
| no -> done?


1. split @ (
  1.1 keep splittin @ (
  1.2 when u find ) return
2. what to do @ , or after ) returns?
