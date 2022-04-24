open Lang

(** Look at input-output types to construct the right catamorphism **)

let check_types (t : typ) (io_examples : exp list) =
  let check_io_types (io_examples : exp list) =
    match io_examples with
    | [] -> internal_error "No examples" ""
    | EPFun ex_list :: _ -> 
      List.iter
        (fun (e1, e2) -> 
          match e1, e2 with
          | ECtor ("Nil", EUnit), ECtor ("O", EUnit)
          | ECtor ("Nil", EUnit), ECtor ("S", ECtor _)
          | ECtor ("Cons", ETuple [_; _]), ECtor ("O", EUnit)
          | ECtor ("Cons", ETuple [_; _]), ECtor ("S", ECtor _) -> ()
          | _ , _ -> internal_error "Currently unsupported" ""
        )
        ex_list
    | _ -> internal_error "Not an EPFun" ""
  in
  match t with
  | TArr (t1, t2) -> 
    (match t1, t2 with
    | TBase x1, TBase x2 -> 
        Printf.printf "%s, %s\n" x1 x2;
        if x1 = "list" && x2 = "nat" then
          check_io_types io_examples
          (* create_values_map io_examples) *)
        else
          internal_error "Currently unsupported" ""
    | _, _ -> internal_error "Currently unsupported" ""
    )
  | _ -> internal_error "Currently unsupported" ""

let inspect_spec (prog : prog) =
  let decls, synth_prob = prog in
  match decls with
  | [] -> let (_, t, io_examples) = synth_prob in
          check_types t io_examples
  | _ -> internal_error "Non-empty decls" ""