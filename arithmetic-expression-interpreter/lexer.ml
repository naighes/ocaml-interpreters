(*
  lexer/scanner/tokenizer
  Transform text representation into a token list.
*)
type token =
    Tkn_NUM of int
  | Tkn_OP of string
  | Tkn_LPAR
  | Tkn_RPAR
  | Tkn_END ;;
(*
  An exception that will be used when an unexpected symbol is met.
*)
exception ParseError of string * string ;;

let tokenize s =
  let parseNum c = function
    | Tkn_NUM n::tokens' -> Tkn_NUM (int_of_string(c ^ (string_of_int n)))::tokens'
    | tokens -> Tkn_NUM (int_of_string c)::tokens
  in
  let parseChar c tokens = match c with
    | " " -> tokens
    | "(" -> Tkn_LPAR::tokens
    | ")" -> Tkn_RPAR::tokens
    | "+" | "-" | "*" | "/" -> (Tkn_OP c)::tokens
    | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> parseNum c tokens
    | _ -> raise (ParseError("Tokenizer", "Unknown symbol: " ^ c))
  in
  (*
    NOTE:
    by the following recursive call, the visit of the whole string starts by its end
    Example: (3 + 41)
    1st invocation: tokens = [Tkn_END]
    2nd invocation: tokens = [Tkn_RPAR; Tkn_END]
    3rd invocation: tokens = [Tkn_NUM (1); Tkn_RPAR; Tkn_END]
    4th invocation: tokens = [Tkn_NUM (41); Tkn_RPAR; Tkn_END]
    5th invocation: tokens = [Tkn_OP ("+"); Tkn_NUM (41); Tkn_RPAR; Tkn_END]
    6th invocation: tokens = [Tkn_NUM (3); Tkn_OP ("+"); Tkn_NUM (41); Tkn_RPAR; Tkn_END]
    7th invocation: tokens = [Tkn_LPAR; Tkn_NUM (3); Tkn_OP ("+"); Tkn_NUM (41); Tkn_RPAR; Tkn_END]
  *)
  let rec tokenize_rec s pos =
    if pos = String.length s then [Tkn_END]
    else parseChar (String.sub s pos 1) (tokenize_rec s (pos + 1))
in
  tokenize_rec s 0 ;;

let string_of_token = function
  | Tkn_NUM i -> "Tkn_NUM " ^ (string_of_int i)
  | Tkn_OP s -> "Tkn_OP " ^ s
  | Tkn_LPAR -> "Tkn_LPAR"
  | Tkn_RPAR -> "Tkn_RPAR"
  | Tkn_END -> "Tkn_END" ;;
