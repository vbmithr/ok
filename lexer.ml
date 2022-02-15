open Sedlexing

exception KError_syntax

type token =
  (* Identifiers *)
  | NAME of string
  | STRING of string
  | SYSNAME of string
  | INT of int
  | FLOAT of float
  (* One-letter symbols *)
  | DOT
  | FORWARDSLASH | BACKSLASH
  | PLUS | MINUS | STAR
  | EXCLAIM
  | PERCENT
  | PIPE
  | AMPERSAND
  | CIRCUMFLEX
  | LPAREN | RPAREN
  | LANGLE | RANGLE
  | LBRACKET | RBRACKET
  | LBRACE | RBRACE
  | EQUALS
  | POUND
  | LODASH
  | TILDE
  | DOLLAR
  | QUESTION
  | AT
  | COMMA
  | QUOTE
  | BACKTICK
  | COLON
  | SEMI
  (* Two-letter symbols *)
  | FORWARDSLASHCOLON | BACKSLASHCOLON
  | QUOTECOLON
  | COLONCOLON
  (* Misc. *)
  | COMMENTREST
  | WS
  | EOF

let wsp = [%sedlex.regexp? Chars " \t\n\r"]
let lcase = [%sedlex.regexp? 'a' .. 'z']
let ucase = [%sedlex.regexp? 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let ident_start = [%sedlex.regexp? lcase | ucase]
let ident_rest = [%sedlex.regexp? lcase | ucase | digit | '_']
let ident = [%sedlex.regexp? ident_start, Star ident_rest]
let exp = [%sedlex.regexp? ('e'|'E'), Opt ('+'|'-'), Plus digit]
let float = [%sedlex.regexp? Star digit, Opt '.', Plus digit, Opt exp]

let buf = Buffer.create 13

let rec read lexbuf =
  match%sedlex lexbuf with
  | '.' -> DOT
  | '/' -> FORWARDSLASH
  | '\\' -> BACKSLASH
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> STAR
  | '!' -> EXCLAIM
  | '?' -> QUESTION
  | '%' -> PERCENT
  | '|' -> PIPE
  | '&' -> AMPERSAND
  | '^' -> CIRCUMFLEX
  | '(' -> LPAREN
  | ')' -> RPAREN
  | '<' -> LANGLE
  | '>' -> RANGLE
  | '[' -> LBRACKET
  | ']' -> RBRACKET
  | '{' -> LBRACE
  | '}' -> RBRACE
  | '=' -> EQUALS
  | '#' -> POUND
  | '_', ident -> SYSNAME (Latin1.lexeme lexbuf)
  | '_' -> LODASH
  | '~' -> TILDE
  | '$' -> DOLLAR
  | '@' -> AT
  | ',' -> COMMA
  | ';' -> SEMI
  | ':' -> COLON
  | '`' -> BACKTICK
  | '\'' -> QUOTE
  | "/," -> FORWARDSLASHCOLON
  | "\\," -> BACKSLASHCOLON
  | "':" -> QUOTECOLON
  | "::" -> COLONCOLON
  | '"' -> Buffer.clear buf; read_string lexbuf
  | ident -> NAME (Latin1.lexeme lexbuf)
  | Plus digit -> INT (int_of_string (Latin1.lexeme lexbuf))
  | float -> FLOAT (float_of_string (Latin1.lexeme lexbuf))
  | Plus wsp -> WS
  | eof -> EOF
  | _ -> raise KError_syntax

and read_string lexbuf =
  match%sedlex lexbuf with
  | "\\\"" -> Buffer.add_char buf '"'; read_string lexbuf
  | "\\n" -> Buffer.add_char buf '\n'; read_string lexbuf
  | "\\r" -> Buffer.add_char buf '\r'; read_string lexbuf
  | "\\t" -> Buffer.add_char buf '\t'; read_string lexbuf
  | "\\b" -> Buffer.add_char buf '\b'; read_string lexbuf
  | "\\", digit, digit, digit ->
    Buffer.add_char buf (Latin1.sub_lexeme lexbuf 1 3 |> fun s ->int_of_string ("0o" ^ s) |> Char.chr);
    read_string lexbuf
  | "\\x", digit, digit ->
    Buffer.add_char buf (Latin1.sub_lexeme lexbuf 1 2 |> fun s -> int_of_string ("0x" ^ s) |> Char.chr);
    read_string lexbuf
  | '"' -> STRING (Buffer.contents buf)
  | any -> Buffer.add_char buf (Latin1.lexeme_char lexbuf 0); read_string lexbuf
  | _ -> raise KError_syntax
