let name smallest = fun x => fun y => if x < y then x else y endif in
let name largest = fun x => fun y => if x < y then y else x endif in
smallest 5 (largest 1 2) end end