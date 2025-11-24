open Ast
open Types


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************)
(* Big step semantics of expr *)
(******************************)

let rec eval_expr st e =
  match e with
  | True -> Bool true
  | False -> Bool false
  | Var x -> st x
  | Const n -> Nat n
  | Not e -> (
    match eval_expr st e with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "Not")
    )
  | And (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 && b2)
    | _, _ -> raise (TypeError "And")
    )
  | Or (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool (b1 || b2)
    | _, _ -> raise (TypeError "Or")
    )
  | Add (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2  -> Nat (n1+n2) 
    | _ -> raise (TypeError "Add")
    )
  | Sub (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 when n1 >= n2-> Nat (n1-n2)
    | _ -> raise (TypeError "Sub")
    )
  | Mul (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 -> Nat (n1*n2)
    | _ -> raise (TypeError "Mul")
    )
  | Eq (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 -> Bool (n1 = n2)
    | _ -> raise (TypeError "Eq")
    )
  | Leq (e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Nat n1, Nat n2 -> Bool (n1 <= n2)
    | _ -> raise (TypeError "Leq")
    )

(*******************************)
(* Small step semantics of cmd *)
(*******************************)

let bot : state = fun x -> raise (UnboundVar x)

let bind st x v : state = fun y -> if x = y then v else st y

let rec trace1 = function
| St _ -> raise NoRuleApplies
| Cmd(cmd, st) ->
  match cmd with
  | Skip -> St st
  | Assign(x, e) -> St (bind st x (eval_expr st e))
  | Seq(c1, c2) -> (
    match trace1 (Cmd(c1, st)) with
    | St st' -> Cmd(c2, st')
    | Cmd(c1', st') -> Cmd(Seq(c1', c2), st')
    )
  | If(e, c1, c2) -> (
    match eval_expr st e with
    | Bool false -> Cmd(c2, st)
    | Bool true -> Cmd(c1, st)
    | _ -> raise (TypeError "If")
    )
  | While(e, c) -> (
    match eval_expr st e with
    | Bool false -> St st
    | Bool true -> Cmd(Seq(c, While(e,c)), st)
    | _ -> raise (TypeError "While")
  )


