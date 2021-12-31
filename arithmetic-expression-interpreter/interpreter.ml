open Parser
(*

a) n -ss-> n

         E1 -ss-> E1'
b) ------------------------
   E1 op E2 -ss-> E1' op E2

         E2 -ss-> E2'
c) ------------------------
    v op E2 -ss-> v op E2'

     n1 op n2 = n
d) ----------------
   n1 op n2 -ss-> n

*)

let rec eval_ss e =
  let mapOp n1 n2 = function
    | Add -> Val (n1 + n2)
    | Sub -> Val (n1 - n2)
    | Mul -> Val (n1 * n2)
    | Div -> Val (n1 / n2)
  in
  let evalOp e1 e2 op = match (e1, e2) with
    | (Val n1, Val n2) -> mapOp n1 n2 op (* rule "d" *)
    | (Op (_, _, _), _) -> Op (op, (eval_ss e1), e2) (* rule "b" *)
    | (_, Op (_, _, _)) -> Op (op, e1, (eval_ss e2)) (* rule "c" *)
  in
  match e with
  | Val n -> Val n (* rule "a" *)
  | Op (op, e1, e2) -> evalOp e1 e2 op ;;

(*

a) n -bs-> n

   E1 -bs-> n1    E2 -bs-> n2   n1 op n2 = n
b) -----------------------------------------
                E1 op E2 -> n

*)
let rec eval_bs e =
  let mapOp n1 n2 = function
    | Add -> Val (n1 + n2)
    | Sub -> Val (n1 - n2)
    | Mul -> Val (n1 * n2)
    | Div -> Val (n1 / n2)
  in
  match e with
    | Val n -> Val n (* rule "a" *)
    | Op (op, e1, e2) -> 
    match (eval_bs e1, eval_bs e2) with (* rule "b" *)
      | (Val n1, Val n2) -> mapOp n1 n2 op
      | _ -> failwith "Error: cannot happen" ;;
