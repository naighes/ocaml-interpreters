let string_to_list s = List.init (String.length s) (String.get s) ;;

string_to_list Sys.argv.(1) |>
Lexer.tokenize |>
Parser.parse |>
fst |>
Interpreter.reduce |>
Parser.exp_to_string ;;
