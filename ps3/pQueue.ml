type 'a comparator = 'a -> 'a -> int

module type BINHEAP = sig
    exception BinHeapError
    type 'a bh
    val empty : 'a comparator -> 'a bh
    val isempty : 'a bh -> bool
    val insert : 'a -> 'a bh -> 'a bh
    val remove : 'a bh -> ('a * 'a bh) option
    val max : 'a bh -> 'a
    val comparator : 'a bh -> 'a comparator
    (*val test : int bh -> unit*)
end


module type PQ = sig
    exception PqueueError
    type 'a t
    val empty : 'a comparator -> 'a t
    val insert : 'a -> 'a t -> 'a t
    val remove : 'a t -> ('a * 'a t) option
    val max : 'a t -> 'a option
    val size : 'a t -> int
    val is_empty : 'a t -> bool
    val comparator : 'a t -> 'a comparator
end


module BinHeap : BINHEAP = struct
    exception BinHeapError
    type 'a bh = ('a comparator * 'a array)
    (*let test (bh : 'a bh)=
        let (a, b) = bh in
        let rec aux n : unit=
            if n >= Array.length b then () else(
            print_bytes "call ";
            print_int b.(n);
            print_bytes " ";
            aux (n + 1))
        in
        aux 0;
        print_bytes "test done\n"*)

    let heapify (bh : 'a bh) n =
        let cmp = fst bh 
        and bl = snd bh in
        let max3 a b c : int =
            match (cmp a b, cmp a c, cmp b c) with
            | (a, b, c) when a >= 0 && b >= 0 -> 0
            | (a, b, c) when a <= 0 && c >= 0 -> 1
            | (a, b, c) when b <= 0 && c <= 0 -> 2
            | _ -> raise BinHeapError
        in
        (* n start from 1 *)
        let rec aux bl n =
            let blen = Array.length bl in
            match (n - blen, n * 2 - blen) with
            | (a, b) when a >= 0 -> bl
            | (a, b) when b > 0 -> bl
            | (a, b) when b = 0 -> 
                    if cmp (Array.get bl (n - 1)) (Array.get bl (n * 2 - 1)) >= 0 then bl else    
                        let bigger = bl.(n * 2 - 1) in
                        let () = Array.set bl (n * 2 - 1) (Array.get bl (n - 1)) in
                        let () = Array.set bl (n - 1) bigger in
                        aux bl (n * 2)
            | (a, b) -> 
                    let c = Array.get bl (n - 1)
                    and l = Array.get bl (n * 2 - 1)
                    and r = Array.get bl (n * 2) in
                    match max3 c l r with
                    | 0 -> bl
                    | 1 -> 
                        let bigger = bl.(n * 2 - 1) in
                        let () = Array.set bl (n * 2 -1) (Array.get bl (n - 1)) in
                        let () = Array.set bl (n - 1) bigger in
                        aux bl (n * 2)
                    | 2 -> 
                        let bigger = bl.(n * 2) in
                        let () = Array.set bl (n * 2) (Array.get bl (n - 1)) in
                        let () = Array.set bl (n - 1) bigger in
                        aux bl (n * 2 + 1)
                    | _ -> raise BinHeapError
        in
        (cmp, aux bl n)
    let empty (c : 'a comparator) : 'a bh = 
        (c, [||])
    let isempty (bh : 'a bh) : bool =
        match bh with
        | (_, [||]) -> true
        | _ -> false
    let insert (e : 'a) (bh : 'a bh) : 'a bh = 
        let r = (fst bh, Array.append (snd bh) [|e|]) in
        let rec heapifyall (bh : 'a bh) n =
            if n <= 0 then () else
            let _unused = heapify bh n in
            heapifyall bh (n - 1) in
        let () = heapifyall r ((Array.length @@ snd @@ r) / 2) in
        r 
    let max bh = Array.get (snd bh) 0
    let comparator bh = fst bh
    let remove (bh : 'a bh) : ('a * 'a bh) option =
        if isempty bh then None else
        let cmp = fst bh
        and maxe = max bh 
        and last = (snd bh).(Array.length (snd bh) - 1) in
        let () = (snd bh).(0) <- last in
        let sbh = (cmp, Array.sub (snd bh) 0 (Array.length (snd bh) - 1)) in
        let r = heapify sbh 1 in
        Some (maxe, r)
end


module type PP = sig
    type 'a t
    val f : 'a t-> int
    val make : unit -> 'a t
end

module Pq : PP = struct
    type 'a t = (int * int) option
    let make () = Some (1, 1)
    let f a = 2
end
