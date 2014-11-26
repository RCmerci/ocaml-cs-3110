let sum (lst : int list) : int =
    List.fold_left (+) 0 lst
;;
let rev (lst : 'a list) : 'a list = 
    List.fold_left (fun r a -> a :: r) [] lst
;;
let max2 (lst : 'a list) : int =
    match List.length lst with
      0 -> raise (Failure "max2: Fewer than two distinct elements")
    | 1 -> raise (Failure "max2: Fewer than two distinct elements")
    | _ -> 
          let h = List.hd lst in
          if List.exists (fun f -> f <> h) (List.tl lst) then
              List.nth (List.fold_left (fun r f -> 
                  match r with
                    [] -> [f]
                  | [f1] -> if f1 < f then [f; f1] else [f1; f]
                  | [f1; f2] -> ( 
                        match (f1 > f, f2 > f, f1 = f, f2 = f) with
                          (true, true, _, _) -> [f1; f2]
                        | (true, false, _, false) -> [f1; f]
                        | (true, false, _, true) -> [f1; f2]
                        | (false, _, false, _) -> [f; f1]
                        | (false, _, true, _) -> [f1; f2]
                        )
                  | _ -> raise (Failure "not match")
              ) [] lst) 1
              else raise (Failure "max2: Fewer than two distinct elements")
;;

              
