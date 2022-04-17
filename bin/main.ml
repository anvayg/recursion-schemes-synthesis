open Lib

exception Arg_exception

type driver_mode =
  | Default
  | Parse

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
            let _ = Parsercontainer.parse_file f in Printf.printf "Going to parser!"
        | Default -> ignore f
        end
      else 
        Arg.usage args ("File not found: " ^ f)

let () = print_endline "Hello, World!"; main ()