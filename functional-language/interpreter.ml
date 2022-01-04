type label = Lab of string ;;

let string_of_label (Lab l) = l ;;

type id = string ;;

(* polymorphic environment *)
type 't env = id -> 't ;;

(*
  binds the id "x" to the value "v" into the environment "e"
*)
let bind env x v = fun i -> if i = x then v else (env i) ;;

exception FieldNotFound of string ;;

exception TypeMismatch ;;

(* syntax level (AST) *)
type exp =
  | CstInt of int
  | CstTrue
  | CstFalse
  | IsZero of exp
  | Eq of exp * exp
  | Sum of exp * exp
  | Sub of exp * exp
  | Times of exp * exp
  | Or of exp * exp
  | And of exp * exp
  | Den of id
  | IfThenElse of exp * exp * exp
  | Let of id * exp * exp
  | Fun of id * exp
  | Apply of exp * exp
  | LetRec of id * id * exp * exp
  | Record of (label * exp) list
  | Select of exp * label ;;
(*
  | Tuple of exp list
  | TupleGet of exp * exp
*)

(* runtime *)
type evT = 
  | Int of int 
  | Bool of bool
  | Closure of id * exp * evT env
  | RecClosure of id * id * exp * evT env
  | Record of (label * evT) list
  | Unbound ;;

exception EvalError of string ;;

type tname = TInt | TBool ;;

(*
  Check wheter a descriptor d is of type t.
  For example:
  typecheck TInt (Int 5) := true
  typecheck TBool (Int 5) := false
*)
let typecheck t d = match t with
  | TInt -> (match d with
    | Int u -> true
    | _ -> false)
  | TBool -> (match d with
    | Bool u -> true
    | _ -> false) ;;

let is_zero x = match (typecheck TInt x, x) with
  | (true, Int y) -> Bool (y = 0)
  | _ -> failwith ("runtime error") ;;

let int_eq x y = match ((typecheck TInt x, x), (typecheck TInt y, y)) with
  | ((true, Int a), (true, Int b)) -> Bool (a = b)
  | _ -> failwith ("runtime error") ;;

let int_plus x y = match ((typecheck TInt x, x), (typecheck TInt y, y)) with
  | ((true, Int a), (true, Int b)) -> Int (a + b)
  | _ -> failwith ("runtime error") ;;

let int_sub x y = match ((typecheck TInt x, x), (typecheck TInt y, y)) with
  | ((true, Int a), (true, Int b)) -> Int (a - b)
  | _ -> failwith ("runtime error") ;;

let int_mul x y = match ((typecheck TInt x, x), (typecheck TInt y, y)) with
  | ((true, Int a), (true, Int b)) -> Int (a * b)
  | _ -> failwith ("runtime error") ;;

let bool_or x y = match ((typecheck TBool x, x), (typecheck TBool y, y)) with
  | ((true, Bool a), (true, Bool b)) -> Bool (a || b)
  | _ -> failwith ("runtime error") ;;

let bool_and x y = match ((typecheck TBool x, x), (typecheck TBool y, y)) with
  | ((true, Bool a), (true, Bool b)) -> Bool (a && b)
  | _ -> failwith ("runtime error") ;;

let if_then_else cond e1 e2 = match (typecheck TBool cond, cond) with
  | (true, Bool a) -> e1
  | (false, Bool a) -> e2
  | _ -> failwith ("runtime error") ;;

let tuple_get_arg arg = match (typecheck TInt arg, arg) with
  | (true, Int a) -> Int a
  | _ -> failwith ("runtime error") ;;

let rec record_lookup (Lab l) = function
  | [] -> raise (FieldNotFound ("field " ^ string_of_label (Lab l) ^ " was not found"))
  | (Lab l', v)::xs -> if l' = l then v else record_lookup (Lab l) xs ;;

let rec eval e env = match e with
  | CstInt n -> Int n
  | CstTrue -> Bool true
  | CstFalse -> Bool false
  | IsZero e -> is_zero (eval e env)
  | Eq (e1, e2) -> int_eq (eval e1 env) (eval e2 env)
  | Sum (e1, e2) -> int_plus (eval e1 env) (eval e2 env)
  | Sub (e1, e2) -> int_sub (eval e1 env) (eval e2 env)
  | Times (e1, e2) -> int_mul (eval e1 env) (eval e2 env)
  | Or (e1, e2) -> bool_or (eval e1 env) (eval e2 env)
  | And (e1, e2) -> bool_and (eval e1 env) (eval e2 env)
  | Den x -> env x
  | IfThenElse (cond, e1, e2) -> eval (if_then_else (eval cond env) e1 e2) env
  | Let (x, e1, e2) -> eval e2 (bind env x (eval e1 env))
  | Fun (x, e) -> Closure (x, e, env)
  | Apply (e_fun, e_arg) ->
    let fclosure = eval e_fun env in (match fclosure with
      (*
        the body is evaluated within an environment which is made
        by fun_dec_env (the environment onto which the function was
        declared) extended with the binding between "x" and the
        argument evaluated into the current environment.
      *)
      | Closure (x, e_body, fun_dec_env) -> eval e_body (bind fun_dec_env x (eval e_arg env))
      | RecClosure (g, x, e_body, fun_dec_env) ->
        let env_arg = bind fun_dec_env x (eval e_arg env) in
        let env_arg_g = bind env_arg g fclosure in
        eval e_body env_arg_g
      | _ -> failwith ("expected closure"))
  (*
    for recursive functions a new environment is built:
    it contains an entry of type RecClosure for "f" itself.
  *)
  | LetRec (f, x, e1, e2) -> eval e2 (bind env f (RecClosure (f, x, e1, env)))
  | Record body -> Record (eval_record body env)
  | Select (e, l) -> match eval e env with
    | Record body -> record_lookup l body
    | _ -> raise TypeMismatch
(*
  | Tuple body -> match body with
    | [] -> []
    | x::xs -> (eval x env)::xs
  | TupleGet (e_tuple, e_arg) ->
    let arg = tuple_get_arg (eval e_arg env) in 
*)
and eval_record body env = match body with
  | [] -> []
  | (Lab l, e)::xs -> (Lab l, eval e env)::eval_record xs env
;;

let x = ("a", 2) in
let y = snd x + 1 in
print_int ;;
(*
  let x = ("a", 2)
  Let(
    "x",
    Tuple(
      ["a", 1]
    ),
    Apply(
      Den("print_int"),
      Den("x")
    )
  )
 *)