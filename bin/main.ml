open Lib

exception Arg_exception

type driver_mode =
  | Default
  | Parse
  | Type
  | Equations

let usage_msg = "main [-help | opts...] <src>"
let filename : string option ref = ref None
let mode : driver_mode ref = ref Default

let set_opt (d:driver_mode) =
  match !mode with
  | Default -> mode := d
  | _ -> raise Arg_exception

let args =
  [("-parse"
    , Arg.Unit (fun _ -> set_opt Parse)
    , " Parse only"
    )
  ; ("-type"
    , Arg.Unit (fun _ -> set_opt Type)
    , "Morphism type"
    )
  ; ("-equations"
    , Arg.Unit (fun _ -> set_opt Equations)
    , "Create constraints"
    )
  ] 
  |> Arg.align

let main () =
  begin try
    Arg.parse args (fun s ->
      match !filename with
      | Some _ -> raise Arg_exception
      | None -> filename := Some s) usage_msg
  with
    Arg_exception -> Arg.usage args usage_msg
  end;
  match !filename with
  | None   -> Arg.usage args usage_msg
  | Some f ->
      if Sys.file_exists f then
        begin match !mode with
        | Parse -> 
            let prog = Parsercontainer.parse_file f in Printf.printf "%s\n" (Pp.pp_prog prog)
        | Type -> 
            ignore (f |> Parsercontainer.parse_file |> Typeinference.inspect_spec)
        | Equations ->
          let prog = Parsercontainer.parse_file f in
          (match Typeinference.inspect_spec prog with
          | ListToNat -> 
            let _, synth_prob = prog in
            let (_, _, io_examples) = synth_prob in
            begin match List.hd io_examples with
            | Lang.EPFun l -> ignore (Equations.create_values_map l)
            | _ -> Printf.printf "Should be unreachable\n"
            end
          | Unknown -> Printf.printf "Not implemented\n"
          )
        | Default -> ignore f (* TODO: change later *)
        end
      else 
        Arg.usage args ("File not found: " ^ f)

let () = print_endline "Hello, World!"; main ()