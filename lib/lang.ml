let real_compare = compare

open Core

(** Language: borrowed from Myth **)
let compare = real_compare

type id = string
[@@deriving eq, ord, show, hash]

type label = string
[@@deriving eq, ord, show, hash]

type 'a record = (label * 'a) list
[@@deriving eq, ord, show, hash]

type typ =
  | TBase of id
  | TArr  of typ * typ
  | TTuple of typ list (* Invariant: List must always have two members. *)
  | TRcd of typ record
  | TUnit
[@@deriving eq, ord, show, hash]

module MType =
struct
  type t = typ
  [@@deriving eq, hash, ord, show]
end

type ctor = id * typ
[@@deriving ord, show, hash]

module Ctor = struct
  type t = ctor
  [@@deriving ord, show, hash]
end

type pattern =
  | PWildcard
  | PVar of id
  | PTuple of pattern list
  | PRcd of pattern record

type pat = id * (pattern option)   (* (id of constructor, pattern). *)

type arg = id * typ

type env = (id * value) list

and decl =
  | DData of id * ctor list
  | DLet  of id * bool * arg list * typ * exp

and exp =
  | EVar of id
  | EApp of exp * exp
  | EFun of arg * exp
  | ELet of id * bool * arg list * typ * exp * exp
  | ECtor of id * exp
  | EMatch of exp * branch list
  | EPFun of (exp * exp) list
  | EFix of id * arg * typ * exp
  | ETuple of exp list  (* Invariant: List must always have two members. *)
  | EProj of int * exp  (* int is the index of projection of the tuple (1-indexed). *)
  | ERcd of exp record
  | ERcdProj of (label * exp)
  | EUnit

and branch = pat * exp

and value =
  | VCtor  of id * value
  | VFun   of id * exp * env ref
  | VPFun  of (value * value) list
  | VTuple of value list (* Invariant: List must always have two members. *)
  | VRcd of value record
  | VUnit

module Type = struct
  type t = typ
  [@@deriving ord, show, hash]
end

let compare_exp (e1:exp) (e2:exp) : int =
  compare e1 e2

let compare_decl (d1:decl) (d2:decl) : int =
  compare d1 d2

type synth_problem = id * typ * exp list

type world = env * value

type prog = decl list * synth_problem

