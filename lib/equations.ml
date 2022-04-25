open Lang
open Catamorphisms
open Typeinference
(* open Format *)
open Pp

(* Simple functions *)

(* identity *)
let id_func = DLet ("id", false, ("n", TBase "nat")::[], (TBase "nat"), (EVar "n"))

(* sucessor function *)
let simple_succ_func = DLet ("simple_succ_func", false, [("n", TBase "nat")], (TBase "nat"), (ECtor ("S", EVar "n")))


(* Create constraints for a list to nat catamorphism *)

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

type expr = 
| EOp of id
| EApp of expr * expr
| ETuple of expr list
| EVal of vexpr

and vexpr =
| VList of nat list
| VNat of nat
[@@deriving ord, show]

type equation = expr * expr (* invariant: 1st proj is LHS, 2nd proj is RHS *)

(* TODO: write pp for expr, based on pp.ml *)
let prec_of_expr (e : expr) : int =
  match e with
  | EOp _ -> 1000
  | EApp _ -> 500
  | ETuple _ -> 700
  | EVal _ -> 1000

let prec_of_vexpr : int = 600

let rec fpf_expr ppf ((lvl, e) : int * expr) =
  let this_lvl = prec_of_expr e in
    (if this_lvl < lvl then fpf ppf "(");
  match e with
  | EOp x -> fpf ppf "%a" ident x
  | EApp (e1, e2) -> 
    fpf ppf "@[<2>%a@ %a@]"
          fpf_expr (this_lvl, e1) fpf_expr (this_lvl + 1, e2)
  | ETuple _ -> ()
  | EVal _ -> ()

and fpf_vexpr ppf ((lvl, v) : int * vexpr) =
    let this_lvl = prec_of_vexpr in
    (if this_lvl < lvl then fpf ppf "(");
    match v with
    | VList _ -> ()
    | VNat _ -> ()

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

let construct_equations (values_map : nat NatListMap.t) (equation_type : morphism_type) : equation list =
  let mk_equation (key : nat list) (value : nat) (fn_string : string) (op_string : string) : equation =
    let lhs = EApp (EOp fn_string, EVal (VList key)) in
    match key with
    | [] -> let rhs = EVal (VNat value) in
            (lhs, rhs)
    | hd :: tl -> let rhs = EApp (EOp op_string, ETuple [EVal (VNat hd); EVal (VList tl)]) in
            (lhs, rhs)
    in
  match equation_type with
  | ListToNat -> 
    let fn_string = "f" in
    let op_string = "plus" in
    NatListMap.fold (fun k v l -> (mk_equation k v fn_string op_string) :: l) values_map []

  | Unknown -> internal_error "Currently unsupported" ""

let saturate_equations (values_map : nat NatListMap.t) (equations : equation list) : equation list = 
  let rec saturate (e : expr) =
    match e with
    | EOp _ -> e
    | EApp (e1, e2) -> EApp (saturate e1, saturate e2)
    | ETuple l -> ETuple (List.map saturate l)
    | EVal v -> 
      begin match v with
      | VNat _ -> EVal v
      | VList l -> 
        (match NatListMap.find_opt l values_map with
        | Some n -> EVal (VNat n)
        | None -> EVal (VList l)
        )
    end
  in
  List.map (fun (lhs, rhs) -> (saturate lhs, saturate rhs)) equations