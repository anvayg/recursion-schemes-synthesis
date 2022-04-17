open Lang

let parse_file (f:string) : prog =
  f |> Lexing.from_string
    |> Parser.prog Lexer.token