open Lang
open Catamorphisms

(* Write catamorphisms using lang.ml *)

(* Simple functions *)

(* identity *)
let id_func = DLet ("id", false, ("n", TBase "nat")::[], (TBase "nat"), (EVar "n"))

(* sucessor function *)
let simple_succ_func = DLet ("simple_succ_func", false, [("n", TBase "nat")], (TBase "nat"), (ECtor ("S", EVar "n")))



(* Specification for a list to nat catamorphism *)
type equation = exp * exp (* invariant: 1st proj is LHS, 2nd proj is RHS *)

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

(* module type CataSpec = functor (M : Map.S) -> sig
    type t = M
    val spec : equation list
end

module CataSpecMap : CataSpec = functor (M : Map.S) -> struct
  type t = M
  let spec : equation list = []
  let add_equation (l : equation list) (eq: equation) : equation list = eq :: l
end

module NatListCataSpec = CataSpecMap(NatListMap) *)


(* Create constraints from cata_spec *)
let create_values_map (io : (exp * exp) list) = 
  let values_map = NatListMap.empty in
  List.fold_left
    (fun m _ -> m (* TODO *)
    ) 
    values_map
    io

let saturate_io_examples (io : equation list) = ignore io