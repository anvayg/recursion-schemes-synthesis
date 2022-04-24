open Lang
open Catamorphisms

(* Write catamorphisms using lang.ml *)

(* Simple functions *)

(* identity *)
let id_func = DLet ("id", false, ("n", TBase "nat")::[], (TBase "nat"), (EVar "n"))

(* sucessor function *)
let simple_succ_func = DLet ("simple_succ_func", false, [("n", TBase "nat")], (TBase "nat"), (ECtor ("S", EVar "n")))


(* Create constraints for a list to nat catamorphism *)

type expr = 
| EOp of id
| EApp of expr * expr
| EVal of vexpr

and vexpr =
| EList of nat list
| ENat of nat

type equation = expr * expr (* invariant: 1st proj is LHS, 2nd proj is RHS *)


module NatList = 
  struct
    type t = nat list
    let rec compare (l : nat list) (ll : nat list) = 
      match (l, ll) with
      | [], [] -> 0
      | [],_ -> -1
      | _,[] -> 1
      | (h::t), (hh::tt) -> 
        begin match compare_nat h hh with
        | 1 -> 1
        | -1 -> -1 
        | _ -> compare t tt
      end

    end

module NatListMap = Map.Make(NatList)

(* Convert ECtor nat to Nat *)
let rec cvt_ctor_to_nat (e : exp) : nat =
  match e with
  | ECtor ("O", EUnit) -> Zero
  | ECtor ("S", e') -> Succ (cvt_ctor_to_nat e')
  | _ -> internal_error "Not a nat" ""

let rec cvt_ctor_to_list (e : exp) : nat list =
  match e with
  | ECtor ("Nil", EUnit) -> []
  | ECtor ("Cons", ETuple [e1;e2]) -> (cvt_ctor_to_nat e1) :: (cvt_ctor_to_list e2)
  | _ -> internal_error "Not a list" ""

(* Invariant: 1st proj is ECtor list and 2nd proj is ECtor nat *)
let create_values_map (io : (exp * exp) list) = 
  let values_map = NatListMap.empty in
  List.fold_left
    (fun m example -> 
      let i, o = example in
      let i', o' = cvt_ctor_to_list i, cvt_ctor_to_nat o in
      NatListMap.add i' o' m
    ) 
    values_map
    io

let saturate_io_examples (io : equation list) = ignore io