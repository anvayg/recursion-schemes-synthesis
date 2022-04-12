
(* Open questions:
    1. How to implement a generic catamorphism module? 
      This module should ideally take a datatype, and return a catamorphism over it of the kind below.
    2. How to use deriving?
*)


(* Experimenting with some basic catamorphisms *)

type nat = 
  | Zero 
  | Succ of nat

let rec cataNat (n : nat) (zero_case : 'a) (succ_case : 'a -> 'a) : 'a =
  match n with
  | Zero -> zero_case
  | Succ(n') -> succ_case (cataNat n' zero_case succ_case)

(* Instantiate cataNat for simple successor function *)
let successor n = cataNat n Zero (fun n -> Succ n)

