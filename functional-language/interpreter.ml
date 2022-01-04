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

exception OutOfRangeIndex ;;

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
  | Select of exp * label
  | Tuple of exp list
  | TupleGet of exp * exp ;;

(* runtime *)
type evT = 
  | Int of int 
  | Bool of bool
  | Closure of id * exp * evT env
  | RecClosure of id * id * exp * evT env
  | Record of (label * evT) list
  | Tuple of evT list
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

let rec record_lookup (Lab l) = function
  | [] -> raise (FieldNotFound ("field " ^ string_of_label (Lab l) ^ " was not found"))
  | (Lab l', v)::xs -> if l' = l then v else record_lookup (Lab l) xs ;;

let tuple_lookup arg e =
  let rec tuple_lookup_aux body a i = match body with
    | [] -> raise OutOfRangeIndex
    | x::xs -> if a = i then x else tuple_lookup_aux xs a (i + 1)
  in (match (typecheck TInt arg, arg) with
    | (true, Int a) -> tuple_lookup_aux e a 1
    | _ -> failwith ("runtime error")) ;;

let rec eval env = function
  | CstInt n -> Int n
  | CstTrue -> Bool true
  | CstFalse -> Bool false
  | IsZero e -> is_zero (eval env e)
  | Eq (e1, e2) -> int_eq (eval env e1) (eval env e2)
  | Sum (e1, e2) -> int_plus (eval env e1) (eval env e2)
  | Sub (e1, e2) -> int_sub (eval env e1) (eval env e2)
  | Times (e1, e2) -> int_mul (eval env e1) (eval env e2)
  | Or (e1, e2) -> bool_or (eval env e1) (eval env e2)
  | And (e1, e2) -> bool_and (eval env e1) (eval env e2)
  | Den x -> env x
  | IfThenElse (cond, e1, e2) -> eval env (if_then_else (eval env cond) e1 e2)
  | Let (x, e1, e2) -> eval (bind env x (eval env e1)) e2
  (*
    Fun (x, e)
    Returns a Closure; it grabs the declaration environment which appears as
    the Closure's third parameter.
  *)
  | Fun (x, e) -> Closure (x, e, env)
  (*
    Apply (e1, e2)
    e1: expected to be a Closure or a RecClosure.
    e2: the argument to be passed as the formal parameter.
  *)
  | Apply (e1, e2) ->
    let fclosure = eval env e1 in (match fclosure with
      (*
        the body is evaluated within an environment which is made
        by fun_dec_env (the environment onto which the function was
        declared) extended by the binding between "x" and the
        argument evaluated into the current environment.
      *)
      | Closure (x, e, fun_dec_env) -> eval (bind fun_dec_env x (eval env e2)) e
      | RecClosure (g, x, e, fun_dec_env) ->
        let env_arg = bind fun_dec_env x (eval env e2) in
        let env_arg_g = bind env_arg g fclosure in
        eval env_arg_g e
      | _ -> failwith ("expected closure"))
  (*
    for recursive functions a new environment is built:
    it contains an entry of type RecClosure for "f" itself.
  *)
  | LetRec (f, x, e1, e2) -> eval (bind env f (RecClosure (f, x, e1, env))) e2
  | Tuple e -> Tuple (eval_tuple env e)
  | TupleGet (e1, e2) -> (match eval env e1 with
    | Tuple e -> tuple_lookup (eval env e2) e
    | _ -> raise TypeMismatch)
  | Record e -> Record (eval_record env e)
  | Select (e, l) -> (match eval env e with
    | Record e -> record_lookup l e
    | _ -> raise TypeMismatch)

and eval_record env = function
  | [] -> []
  | (Lab l, e)::xs -> (Lab l, eval env e)::eval_record env xs
and eval_tuple env = function
  | [] -> []
  | x::xs -> (eval env x)::eval_tuple env xs
;;
