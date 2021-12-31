(*
  lexer/scanner/tokenizer
  transform text representation into a token list
  Remarks: it does not check for grammar correctness
*)

type token =
    Tkn_ABS
  | Tkn_ID of string
  | Tkn_DOT
  | Tkn_LPAR
  | Tkn_RPAR
  | Tkn_END ;;
(*
  let's define an exception that we'll use when an unexpected symbol is met
*)
exception ParseError of string * string ;;

(*
  utility function: string representation for a token
*)
let string_of_token = function
  | Tkn_ID i -> "Tkn_ID " ^ i
  | Tkn_ABS -> "Tkn_ABS"
  | Tkn_DOT -> "Tkn_DOT"
  | Tkn_LPAR -> "Tkn_LPAR"
  | Tkn_RPAR -> "Tkn_RPAR"
  | Tkn_END -> "Tkn_END" ;;

let rec tokenize = function
  | ('@'::xs) -> Tkn_ABS::(tokenize xs)
  | ('('::xs) -> Tkn_LPAR::(tokenize xs)
  | (')'::xs) -> Tkn_RPAR::(tokenize xs)
  | ('.'::xs) -> Tkn_DOT::(tokenize xs)
  | (' '::xs) -> tokenize xs
  | (('a' .. 'z') as x::xs) -> tokenize_id (x::xs)
  | (x::_) -> raise (ParseError("Lexer", "Unknown symbol: " ^ Char.escaped x))
  | [] -> []
and tokenize_id s =
  let rec tokenize_id_aux = function
    | [] -> ("", [])
    | (('@' | '(' | ')' | '.' | ' ' as x)::xs) -> ("", (x::xs))
    | (('a' .. 'z' as x)::xs) -> let (left, right) = tokenize_id_aux xs in (Char.escaped x ^ left, right)
    | (x::_) -> raise (ParseError("Lexer", "Unknown symbol: " ^ Char.escaped x))
in
let (left, right) = tokenize_id_aux s in (Tkn_ID left)::(tokenize right);;
