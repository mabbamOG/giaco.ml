(* implementation of BOOLEAN operations for my interpreter *)
let _or = function
| Bool a,Bool b -> Bool(a||b)
| _ -> failwith "or operation only supported for bool*bool"

let _and = function
| Bool a,Bool b -> Bool(a&&b)
| _ -> failwith "and operation only supported for bool*bool"

let xor = function
| Bool a,Bool b -> Bool(a<>b)
| _ -> failwith "xor operation only supported for bool*bool"

let _not = function
| Bool b -> Bool(not b)
| _ -> failwith "not operation only supported for bool"
