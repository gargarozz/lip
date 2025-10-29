open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* big step semantics for expr *)

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var x -> st x
  | Const n -> Nat n
  | Not(e) -> (match eval_expr e with
      Bool b -> Bool(not b)
      |_ -> raise (TypeError "Not on nat")
    )
  | And(e1,e2) -> (match (eval_expr e1, eval_expr e2) with
      (Bool b1, Bool b2) -> Bool (b1 && b2)
      |_ -> raise (TypeError "And on nat")
    )
  | Or(e1,e2) -> (match (eval_expr e1, eval_expr e2) with
      (Bool b1, Bool b2) -> Bool (b1 || b2)
      |_ -> raise (TypeError "Or on nat")
    )
  | Add(e1,e2) -> (match (eval_expr e1, eval_expr e2) with
      (Nat b1, Nat b2) -> Nat (b1 + b2)
      |_ -> raise (TypeError "Add on bool")
  | Sub(e1,e2) -> (match (eval_expr e1, eval_expr e2) with
      (Nat b1, Nat b2) -> Nat (b1 - b2)
      |_ -> raise (TypeError "Sub on bool")
  | Mul(e1,e2) -> (match (eval_expr e1, eval_expr e2) with
      (Nat b1, Nat b2) -> Nat (b1 * b2)
      |_ -> raise (TypeError "Mul on bool")

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
 
let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 e in Succ(e')
  | Pred(Succ e) when (is_nv e) -> e
  | Pred(e) -> let e' = trace1 e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when (is_nv e) -> False
  | IsZero(e) -> let e' = trace1 e in IsZero(e')
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]



(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

exception TypeError of string

;;
