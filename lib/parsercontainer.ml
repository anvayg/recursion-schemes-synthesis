open Lang
open Core

let read_to_string (filename : string) : string =
  In_channel.read_all filename

let parse_file (f:string) : prog =
  f |> read_to_string
    |> Lexing.from_string
    |> Parser.prog Lexer.token