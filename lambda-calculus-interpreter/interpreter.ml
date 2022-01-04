open Parser
(*
  Semantics:
  NOTE: the semantics is nondeterministic, the
        implementation is not.
  -----------------------------------------------------
  i) (λx.e1)e2 -> e1{x:=e2}

          e1 -> e1'
  ii) ---------------
      e1 e2 -> e1' e2

           e2 -> e2'
  iii) ---------------
       e1 e2 -> e1 e2'

         e -> e'
  iv) -------------
      λx.e -> λx.e'

  NOTE: In call-by-name the beta-reduction rule does not evaluate e2.
        It just make a substitution at syntax level.
*)

(*
  Free variables:
  -----------------------------------------------------
  FV(x)       = {x}
  FV(M N)     = FV(M) U FV(N)
  FV(λx.M)    = FV(M) \ {x}
*)
let rec fv = function
  | Var x -> [x]
  | App (e1, e2) -> fv(e1) @ fv(e2)
  | Lam (x, e) -> List.filter (fun y -> y <> x) (fv e) ;;

let fresh_var =
  let x = ref 0
in
fun () ->
  let c = !x in
  incr x;
  "v"^(string_of_int c) ;;

(*

  Substitutes any occurrence of y by m into within the expression e.

  Capture-avoiding Substitution:
  -----------------------------------------------------
  a)   x{x := e}       = e
  b)   y{x := e}       = y if y <> x
  c)   (e1 e2){x := e} = (e1{x := e} e2{x := e})
  d)   (λy.e1){x := e} = λy.(e1{x := e}) if y <> x and y ∉ FV(e)
  e)   (λy.e1){x := e} = λz.((e1{y := z}){x := e}) if y <> x and y ∈ FV(e) and z is "fresh"

*)
let rec subst e y m =
  match e with
    | Var x -> if y = x then m (* rule "a" *) else e (* rule "b" *)
    | App (e1, e2) -> App ((subst e1 y m), (subst e2 y m)) (* rule "c" *)
    | Lam (x, e) ->
      if y = x then Lam (x, e) (* no need for changes *)
      (* x does not occur as a free variable of m: no alpha-conversion is required *)
      else if not (List.mem x (fv m)) then Lam (x, subst e y m)
      (*
        x occurs as a free variable of m -> replace x by
        a new fresh variable "z": alpha-conversion is required
      *)
      else
        let z = fresh_var () in
        let e' = subst e x (Var z) in
        Lam (z, subst e' y m) ;;

let rec reduce e = match e with
  (* rule "i" *)
  | App (Lam (x, e1), e2) -> subst e1 x e2
  (* rule "iv" *)
  | Lam (x, e) -> Lam (x, reduce e)
  (* rule "ii" and "iii" *)
  | App (e1, e2) ->
    let e1' = reduce e1 in (if e1 <> e1' then App (reduce e1', e2) else App (e1', reduce e2))
  | _ -> e ;;
