type exp = Val of int | Plus of exp * exp | Times of exp * exp ;;

let example_exp =
    Times (
        Plus ( Val 1, Val 1 ),
        Plus (
            Times ( Val 3, Val 4 ),
            Val 1
        )
    );;

let rec exp_fold (val_op : int -> 'a) 
             (plus_op : 'a -> 'a -> 'a)
             (times_op : 'a -> 'a -> 'a)
             (exp : exp) : 'a =
    match exp with 
    | Val v -> val_op v
    | Plus (v1, v2) -> plus_op (exp_fold val_op plus_op times_op v1)
                               (exp_fold val_op plus_op times_op v2)
    | Times (v1, v2) -> times_op (exp_fold val_op plus_op times_op v1)
                                 (exp_fold val_op plus_op times_op v2)
;;

let eval (e : exp) : int =
    exp_fold (fun i -> i) (fun a b -> a + b) (fun a b -> a * b) e
;;

let to_string (e : exp) : string =
    exp_fold (fun i -> string_of_int i) (fun a b -> "(" ^ a ^ "+" ^ b ^ ")")
        (fun a b -> "(" ^ a ^ "*" ^ b ^ ")") e
;;
