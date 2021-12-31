open Parser
open Interpreter

let print_token t = print_endline (Lexer.string_of_token t) ;;
let print_tokens tokens = List.iter print_token tokens ;;

let exec_bs s = eval_bs (parse s) ;;
let exec_ss s =
  let rec exec_ss_rec ast = match ast with (* transitive closure *)
    | Val n -> Val n
    | _ -> exec_ss_rec (eval_bs ast)
  in
  exec_ss_rec (parse s) ;;

(* test with small step interpreter *)
print_endline (exp_to_string (exec_bs Sys.argv.(1))) ;;
