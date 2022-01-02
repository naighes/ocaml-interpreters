open Lexer

(*
  Grammar:
  -----------------------------------------------------
  Exp ::= n | Exp op Exp | (Exp)
  op  ::= + | - | * | /

  Non-ambiguous grammar (also enforces *,/ precedence):
  -----------------------------------------------------
  Exp ::= Term | Term + Exp | Term - Exp
  Term ::= Factor | Factor * Term | Factor / Term
  Factor ::= n | (Exp)

  Another format:
  -----------------------------------------------------
  Exp ::= Term [ + Exp | - Exp ]
  Term ::= Factor [ * Term | / Term ]
  Factor ::= n | (Exp)

  Basically n expression is a sum of terms.
  Example: (3 * 7) - 5
  let exp1 = Op(Sub, Op(Mul, Val 3, Val 7), Val 5)
*)

(* An AST for this grammar *)
type op = Add | Sub | Mul | Div ;;
type exp = Val of int | Op of op * exp * exp ;;

(* Helper function to translates the AST into a string. *)
let rec exp_to_string =
  let op_to_string = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
  in function
    | Val n -> string_of_int n
    | Op (o, e1, e2) -> "(" ^ (exp_to_string e1) ^ op_to_string(o) ^ (exp_to_string e2) ^ ")" ;;

let parse s =
  let tokens = ref (tokenize s) in
  (*
    Peek a token without removing it from the list.
    Remarks: once the token is inspected, build the node of the AST accordingly.
  *)
  let lookahead () = match !tokens with
    | [] -> raise (ParseError("Parser", "Lookhead error."))
    | t::_ -> t in
  (*
    The same as lookahead, but the element is removed.
    Remarks: once the token is inspected, build the node of the AST accordingly.
  *)
  let consume () = match !tokens with
    | [] -> raise (ParseError("Parser", "Consume error."))
    | x::xs -> tokens:= xs
  in

  (*
    NOTE: the "and" keyword allows the following
          function to act in a mutually recursive manner
  *)

  (* Exp ::= Term [ + Exp | - Exp ] *)
  let rec exp () =
    let t1 = term() in
    match lookahead () with
    | Tkn_OP "+" -> consume(); Op (Add, t1, exp())
    | Tkn_OP "-" -> consume(); Op (Sub, t1, exp())
    | _ -> t1
  (* Term ::= Factor [ * Term | / Term ] *)
  and term () =
    let f1 = factor() in
    match lookahead() with
    | Tkn_OP "*" -> consume(); Op (Mul, f1, term())
    | Tkn_OP "/" -> consume(); Op (Div, f1, term())
    | _ -> f1
  (* Factor ::= n | (Exp) *)
  and factor () =
    match lookahead() with
      | Tkn_NUM n -> consume(); Val n
      | Tkn_LPAR -> consume(); let e = exp() in
        (match lookahead() with
          | Tkn_RPAR -> consume(); e
          | _ -> raise (ParseError("Parser", "RPAR error."))
        )
      | _ -> raise (ParseError("Parser", "NUM/LPAR error."))
  in
  let ast = exp() in
  match lookahead() with
    | Tkn_END -> ast
    | x -> print_tokens !tokens; raise (ParseError("ParseError", "END error.")) ;;
