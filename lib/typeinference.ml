open Lang

(** Look at input-output types to construct the right catamorphism **)

let construct_morphism (t : typ) =
  match t with
  | TArr (t1, t2) -> 
    (match t1, t2 with
    | TBase x1, TBase x2 -> 
        Printf.printf "%s, %s\n" x1 x2
    | _, _ -> internal_error "Currently unsupported" ""
    )
  | _ -> internal_error "Currently unsupported" ""

let inspect_spec (prog : prog) =
  let decls, synth_prob = prog in
  match decls with
  | [] -> let (_, t, _) = synth_prob in
          construct_morphism t
  | _ -> internal_error "Non-empty decls" ""