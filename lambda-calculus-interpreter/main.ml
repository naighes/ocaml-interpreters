let toCharList s = List.init (String.length s) (String.get s) ;;
let print_token t = print_endline (Lexer.string_of_token t) ;;
let print_tokens tokens = List.iter print_token tokens ;;

(* tokenize *)
let tokens = Lexer.tokenize (toCharList Sys.argv.(1)) ;;

(* print tokens *)
Lexer.print_tokens tokens ;;

print_endline ""

(* build the ast *)
let ast = Parser.parse tokens ;;

(* print an ast string representation *)
print_endline (Parser.exp_to_string (fst ast) );;
