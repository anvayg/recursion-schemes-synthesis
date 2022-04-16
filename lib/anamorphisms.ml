
let rec anaList (init_case : 'a) (next_case : 'a -> ('a * 'b) option) : 'b list =
  match next_case init_case with
  | None -> []
  | Some (y, v) -> v :: (anaList y next_case)