(* implementation of NUMBER operations for my interpreter *)
let add = function
| Int a, Int b -> Int (a+b)
| Float a, Float b -> Float (a+.b)
| _ -> failwith "add operation only supported for int*int or float*float"

let mul = function
| Int a,Int b -> Int(a*b)
| Float a, Float b -> Float(a*.b)
| _ -> failwith "mul operation only supported for int*int or float*float"

let sub = function
| Int a,Int b -> Int(a-b)
| Float a, Float b -> Float(a-.b)
| _ -> failwith "sub operation only supported for int*int or float*float"

let div = function
| Int a,Int b -> Int(a/b)
| Float a, Float b -> Float(a/.b)
| _ -> failwith "div operation only supported for int*int or float*float"
