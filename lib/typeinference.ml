open Lang
open CatamorphismsLang

(** Look at input-output types to construct the right catamorphism **)

let construct_morphism (t : typ) (io_examples : exp list) =
  match t with
  | TArr (t1, t2) -> 
    (match t1, t2 with
    | TBase x1, TBase x2 -> 
        Printf.printf "%s, %s\n" x1 x2;
        if x1 = "list" && x2 = "nat" then
          (* check ECtor : should be exp * exp, with LHS being ECtor list and RHS ECtor nat *)
          create_values_map io_examples
        else
          internal_error "Currently unsupported" ""
    | _, _ -> internal_error "Currently unsupported" ""
    )
  | _ -> internal_error "Currently unsupported" ""

let inspect_spec (prog : prog) =
  let decls, synth_prob = prog in
  match decls with
  | [] -> let (_, t, io_examples) = synth_prob in
          construct_morphism t io_examples
  | _ -> internal_error "Non-empty decls" ""