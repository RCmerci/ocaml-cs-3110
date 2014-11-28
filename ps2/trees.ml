type 'a bintree = Leaf | Node of 'a bintree * 'a * 'a bintree

let rec tree_sum (t : int bintree) : int =
    match t with
    | Leaf -> 0
    | Node (t1, v, t2) -> v + (tree_sum t1) + (tree_sum t2)
;;

let example_tree = 
    Node (
        Node (
            Node (Leaf, 2, Leaf),
            4, 
            Node (Leaf,
                7,
                Node (Leaf, 8, Leaf))),
        3110,
        Node (
            Leaf,
            6,
            Node (Node (Leaf, -3, Leaf),
                14,
                Leaf)));;

let rec tree_mem e (t : 'a bintree) : bool =
    match t with
    | Leaf -> false
    | Node (t1, v, t2) -> if v = e then true else tree_mem e t1 || tree_mem e t2
;;

let rec tree_preorder (t : 'a bintree) : 'a list =
    match t with
    | Leaf -> []
    | Node (t1, v, t2) -> v :: tree_preorder t1 @ tree_preorder t2
;;

let rec tree_inorder (t : 'a bintree) : 'a list =
    match t with
    | Leaf -> []
    | Node (t1, v, t2) -> tree_inorder t1 @ [v] @ tree_inorder t2
;;

let rec tree_postorder (t : 'a bintree) : 'a list =
    match t with
    | Leaf -> []
    | Node (t1, v, t2) -> tree_postorder t1 @ tree_postorder t2 @ [v]
;;

let rec tree_fold (r : 'b) (f : 'b -> 'a -> 'b -> 'd) (t : 'a bintree) : 'b =
    match t with
    | Leaf -> r
    | Node (t1, v, t2) -> f (tree_fold r f t1) v (tree_fold r f t2)
;;

let tree_sum_fold (t : int bintree) : int =
    tree_fold 0 (fun l x r -> l + 1 + r) t
;;

let tree_mem_fold e (t : 'a bintree) : bool =
    tree_fold false (fun l x r -> x = e || l || r) t
;;

let tree_preorder_fold (t : 'a bintree) : 'a list =
    tree_fold [] (fun l x r -> x :: l @ r) t
;;

let tree_inorder_fold (t : 'a bintree) : 'a list =
    tree_fold [] (fun l x r -> l @ [x] @ r) t
;;

let tree_postorder_fold (t : 'a bintree) : 'a list =
    tree_fold [] (fun l x r -> l @ r @ [x]) t
;;

