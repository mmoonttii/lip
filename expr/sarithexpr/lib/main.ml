open Ast

type exprval = Bool of bool | Nat of int
type exprtype = BoolT | NatT


let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"
;;

let string_of_type = function
      BoolT -> "Bool"
    | NatT -> "Nat"
;;

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

exception TypeError of string

let rec eval (e : expr) : exprval =
  match e with
  | True -> Bool(true)
  | False -> Bool(false)
  | Not(e1) -> (
    match eval e1 with
    | Bool(e') -> Bool(not e')
    | _ -> raise (TypeError "Errore")
    )
  | And(e1, e2) -> (
    match eval e1, eval e2 with
    | Bool(true), Bool(v') -> Bool(v')
    | Bool(false), _ -> Bool(false)
    | _, _ -> raise (TypeError "Errore")
    )
  | Or(e1, e2) -> (
      match eval e1, eval e2 with
      | Bool(true), Bool(_) -> Bool(true)
      | Bool(false), Bool(e') -> Bool(e') 
      | _, _ -> raise (TypeError "Errore")
    )
  | If(e0, e1, e2) -> (
      match eval e0 with
      | Bool(true) -> eval e1
      | Bool(false) -> eval e2
      | _ -> raise (TypeError "Errore")
    )
  | Zero -> Nat(0)
  | Succ(n) -> (
      match eval n with
      | Nat(n) -> Nat(n+1)
      | _ -> raise (TypeError "Errore")
    )
  | Pred(n) -> (
      match eval n with
      | Nat(n) when n > 0 -> Nat(n-1)
      | _ -> raise (TypeError "Errore")
    )
  | IsZero(e') -> (
    match eval e' with
    | Nat(0) -> Bool(true)
    | Nat(_) -> Bool(false)
    | _ -> raise (TypeError "Errore")
  )
;;

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
exception PredOfZero

let rec is_nv = function
| Zero -> true
| Succ(e) -> is_nv e
| _ -> false
;;

let rec trace1 (e : expr) : expr =
  match e with
  | Not(True) -> False
  | Not(False) -> True
  | Not(e1) -> Not(trace1 e1)
  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2)
  | If(True, e1, _) -> e1
  | If(False, _, e2) -> e2
  | If(e0, e1, e2) -> If(trace1 e0, e1, e2)
  | Succ(e1) -> Succ(trace1 e1)
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e1)) when is_nv e1 -> e1
  | Pred(e1) -> Pred(trace1 e1)
  | IsZero(Zero) -> True
  | IsZero(Succ(e1)) when (is_nv e1) -> False
  | IsZero(e1) -> IsZero(trace1 e1)
  | _ -> raise NoRuleApplies
;;

let rec trace (e : expr) : expr list = 
  try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

(******************************************************************************)
(*                            Type-checking                                   *)
(******************************************************************************)

let rec typecheck (e : expr) : exprtype =
  match e with
  | True -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Succ(e) | Pred(e) -> (
    match (typecheck e) with
    | NatT -> NatT
    | BoolT -> raise (TypeError (string_of_expr e ^ " has type Bool, but type Nat was expected"))
    )
  | IsZero(e) -> (
    match (typecheck e) with
    | NatT -> BoolT 
    | BoolT -> raise (TypeError (string_of_expr e ^ " has type Bool, but type Nat was expected"))
    )
  | Not(e) -> (
    match (typecheck e) with
    | BoolT -> BoolT
    | NatT -> raise (TypeError (string_of_expr e ^ " has type Nat, but type Bool was expected"))
    )
  | And(e1, e2) | Or(e1, e2) -> (
    match (typecheck e1, typecheck e2) with
    | BoolT, BoolT -> BoolT
    | NatT, _ -> raise (TypeError (string_of_expr e1 ^ " has type Nat, but type Bool was expected"))
    | _, NatT -> raise (TypeError (string_of_expr e2 ^ " has type Nat, but type Bool was expected"))
    )
  | If(e0, e1, e2) -> (
    match (typecheck e0, typecheck e1, typecheck e2 ) with
      | (NatT, _, _) -> raise (TypeError (string_of_expr e0 ^ " has type Nat, but type Bool was expected"))
      | (BoolT, t1, t2) when t1 = t2 -> t1
      | (BoolT, t1, t2) -> raise (TypeError (string_of_expr e2 ^ " has type " ^ string_of_type t2 ^ ", but type " ^ string_of_type t1 ^ " was expected"))
    )