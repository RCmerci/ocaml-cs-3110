let rec lengths_rec (lst : 'a list list) : int list = 
    let rec length (lst : 'a list) : int =
        match lst with
          [] -> 0
        | h :: t -> length t + 1 in
            match lst with
              [] -> []
            | h :: t -> length h :: lengths_rec t
;;

let lengths_fold (lst : 'a list list) : int list =
    let length (lst : 'a list) : int =
        List.fold_left (fun r f -> r + 1) 0 lst 
    in 
    List.fold_left (fun r f -> f :: r)  [] (List.fold_left (fun r f -> length f :: r) [] lst)
;;

let lengths_lib (lst : 'a list list) : int list =
    List.map (fun f -> List.length f) lst
;;

let rec find_first_value_rec (lst : ('a * 'b) list) (x : 'a) : 'b option =
    match lst with
    | [] -> None
    | (k, v) :: t -> if k = x then Some v else find_first_value_rec t x
;;

let find_first_value_fold (lst : ('a * 'b) list) (x : 'a) : 'b option =
    List.fold_left (fun r (k, v) -> if r <> None then r else 
        if k = x then Some v else r) None lst
;;

let find_first_value_lib (lst : ('a * 'b) list) (x : 'a) : 'b option =
    try 
        match List.find (fun (k, v) -> k = x) lst with 
        | (_, v) -> Some v
    with
    | Not_found -> None
;;

let confirm_outputs (fs : ('a -> 'b) list) (i : 'a) (o : 'b) : bool =
    not (List.exists (fun f -> f i <> o) fs)
;;

let total_length (lsts : 'a list list) : int =
    List.length (List.flatten lsts) 
;;

let find_last_value (lst : ('a * 'b) list) (x : 'a) : 'b option =
    List.fold_left (fun r (k, v) -> if k = x then Some v else r) None lst
;;

let median (lst : 'a list) : 'a option =
    if lst = [] then None else
    Some (List.nth (List.sort compare lst) (((List.length lst) - 1) / 2))
;;

