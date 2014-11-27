type bit = Zero | One
type bits = bit list
;;

let rec bits_to_int (bs : bits) : int =
    let len = List.length bs in
    match bs with
    | [] -> 0
    | Zero :: t -> 0 + bits_to_int t
    | One :: t -> int_of_float (2. ** float_of_int (len - 1)) + bits_to_int t
;;

let rec int_to_bits (i : int) : bits =
    let m = i mod 2 
    and n = i / 2 in
    match (m, n) with
    | (0, 0) -> []
    | (1, 0) -> [One]
    | (0, v) -> int_to_bits v @ [Zero]
    | (1, v) -> int_to_bits v @ [One]
    | _ -> raise (Failure "in int_to_bits")
;;

let binary_addition (n1 : bits) (n2 : bits) : bits =
    List.fold_left (fun r f -> if (r = []) && (f = Zero) then [] else r @ [f]) [] 
       (List.rev
       (let r1 = List.rev n1 
        and r2 = List.rev n2 in
        let rec aux m1 m2 (r : bit) : bits =
            match (m1, m2, r) with
            | (Zero :: t1, Zero :: t2, Zero) -> Zero :: (aux t1 t2 Zero)
            | (Zero :: t1, Zero :: t2, One)  -> One :: (aux t1 t2 Zero)
            | (One :: t1, Zero :: t2, Zero)  -> One :: (aux t1 t2 Zero)
            | (One :: t1, Zero :: t2, One)   -> Zero :: (aux t1 t2 One)
            | (Zero :: t1, One :: t2, Zero)  -> One :: (aux t1 t2 Zero)
            | (Zero :: t1, One :: t2, One)  -> Zero :: (aux t1 t2 One)
            | (One :: t1, One :: t2, Zero)  -> Zero :: (aux t1 t2 One)
            | (One :: t1, One :: t2, One)  -> One :: (aux t1 t2 One)
            | (Zero :: t1, [], Zero)  -> Zero :: (aux t1 [] Zero)
            | (Zero :: t1, [], One)  -> One :: (aux t1 [] Zero)
            | (One :: t1, [], Zero)  -> One :: (aux t1 [] Zero)
            | (One :: t1, [], One)  -> Zero :: (aux t1 [] One)
            | ([], Zero :: t2, Zero)  -> Zero :: (aux [] t2 Zero)
            | ([], Zero :: t2, One)  -> One :: (aux [] t2 Zero)
            | ([], One :: t2, Zero)  -> One :: (aux [] t2 Zero)
            | ([], One :: t2, One)  -> Zero :: (aux [] t2 One)
            | ([], [], v)  -> [v] in
        aux r1 r2 Zero))
    ;;
        
