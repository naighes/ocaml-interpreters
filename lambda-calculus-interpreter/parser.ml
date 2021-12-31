open Lexer

(*
  Grammar:
  ------------------------------
  e ::= x | $x.e | e e
*)

let lambda = "\xCE\xBB" ;;

(* An AST for this grammar *)
type id = string ;;
type exp = Var of id | Lam of id * exp | App of exp * exp ;;

(* Helper function who translates the AST into a string. *)
let rec exp_to_string = function
  | Var x -> x
  | Lam (x, e) -> lambda ^ x ^ "." ^ (exp_to_string e)
  | App (e1, e2) -> (exp_to_string e1) ^ " " ^ (exp_to_string e2) ;;

let rec parentheses tokens =
  let rec parentheses_aux tokens' n =
    let prepend token (left, right) = (token::left, right)
  in match (tokens', n) with
    (* On Tkn_RPAR and n = 1, the body within parentheses is returned *)
    | ((Tkn_RPAR::xs), 1) -> ([], xs)
    (* On Tkn_LPAR increment n *)
    | ((Tkn_LPAR::xs), _) -> parentheses_aux xs (succ n) |> prepend Tkn_LPAR
    (* A Tkn_RPAR with n <> 1, so looking for additional Tkn_RPAR *)
    | ((Tkn_RPAR::xs), _) -> parentheses_aux xs (pred n) |> prepend Tkn_RPAR
    (* No Tkn_LPAR nor Tkn_RPAR, so move ahead *)
    | (x::xs, _) -> parentheses_aux xs (n) |> prepend x
    | _ -> raise (ParseError("Parser", "Detected unbalanced parentheses"))
in parentheses_aux tokens 1 |> (function
  (*
    Parse the body within parentheses as an expression; then a tuple made
    by the expression itself and the remaining tokens is returned
    NOTE: the first token within parentheses must be an abstraction, a further
          Tkn_LPAR or an identifier (e.g. a Tkn_DOT is not allowed by the grammar)
  *)
  | ((Tkn_ABS | Tkn_LPAR | (Tkn_ID _) as x)::xs, tokens') -> parse (x::xs) |> (fun (e, _) -> (e, tokens'))
  | _ -> raise (ParseError("Parser", "The first token within parentheses is expected to be Tkn_ABS | Tkn_LPAR | Tkn_ID")))

and unary_block = function
  | (Tkn_ABS as x::xs) -> abstraction (x::xs)
  | (Tkn_LPAR::xs) -> parentheses xs
  | (Tkn_ID x::xs) -> ((Var x), xs)
  | _ -> raise (ParseError("Parser", "Expected Tkn_ABS | Tkn_LPAR | Tkn_ID"))

and abstraction =
  let abstraction_aux token (e, tokens) = match token with
    (* A Tkn_ID token is expected as formal parameter *)
    | Tkn_ID id -> (Lam (id, e), tokens)
    | _ -> raise (ParseError("Parser", "Expected formal parameter for abstraction; got " ^ string_of_token token ^ " instead"))
in function
  | (Tkn_ABS::token::Tkn_DOT::xs) -> parse xs |> abstraction_aux token
  | _ -> raise (ParseError("Parser", "Bad abstraction format: use @<id>.<exp>"))

and application =
  let application_aux e1 (e2, tokens) = application (App (e1, e2), tokens)
in function
  (* No remaining tokens: just return the expression *)
  | (e, []) -> (e, [])
  (*
    Additional tokens: keep the first member of the application (e1), then
    find e2 and wrap both into a further application
  *)
  | (e1, tokens) -> unary_block tokens |> application_aux e1

and parse tokens = application (unary_block tokens) ;;
