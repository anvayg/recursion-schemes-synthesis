
(* Open questions:
    1. How to implement a generic catamorphism module? 
      This module should ideally take a datatype, and return a catamorphism over it of the kind below.
*)

(* TODO: redefine these catamorphisms in terms of the language in Lang.ml
*)


(* Experimenting with some basic catamorphisms *)

type nat = 
  | Zero
  | Succ of nat
[@@deriving show]

let rec nat_to_int (n : nat) : int =
  match n with
  | Zero -> 0
  | Succ n' -> 1 + (nat_to_int n')

let compare_nat n1 n2 = compare (nat_to_int n1) (nat_to_int n2)

let rec cataNat (n : nat) (zero_case : 'a) (succ_case : 'a -> 'a) : 'a =
  match n with
  | Zero -> zero_case
  | Succ(n') -> succ_case (cataNat n' zero_case succ_case)

(* Instantiate cataNat for simple successor function *)
let cataSucc n = cataNat n (Succ Zero) (fun n -> Succ n)

(* inline tests on successor *)
let%test _ = cataSucc (Succ Zero) = Succ (Succ Zero)


let rec cataList (l : 'a list) (nil_case : 'b) (cons_case : 'a -> 'b -> 'b) : 'b =
  match l with
  | [] -> nil_case
  | hd :: tl ->  cons_case hd (cataList tl nil_case cons_case)


(* Instantiate cataList for length function *)
let cataLength l = cataList l 0 (fun _ b -> 1 + b)

(* TODO: write tests on length function *)
let%test _ = cataLength (1 :: (2 ::[])) = 2