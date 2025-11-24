open Ast
open Types

let apply st x = match topenv st x with
    IVar l -> getmem st l
  | _ -> failwith "apply error"

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)

exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc
exception PredOfZero
exception NoRuleApplies

let botenv = fun x -> raise (UnboundVar x)
let botmem = fun l -> raise (UnboundLoc l) 
    
let bind f x v = fun y -> if y=x then v else f y

let rec trace1_expr st = function
| Var x -> (Const(apply st x), st)
| Not(True) -> (False,st)
| Not(False) -> (True,st)
| Not(e) -> let (e',st') = trace1_expr st e in (Not(e'),st')
| And(True,e) -> (e,st)
| And(False,_) -> (False,st)
| And(e1,e2) -> let (e1',st') = trace1_expr st e1 in (And(e1',e2),st')
| Or(True,_) -> (True,st)
| Or(False,e) -> (e,st)
| Or(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Or(e1',e2),st')
| Add(Const(n1),Const(n2)) -> (Const(n1+n2),st)
| Add(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Add(Const(n1),e2'),st')
| Add(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Add(e1',e2),st')
| Sub(Const(n1),Const(n2)) -> (Const(n1-n2),st)
| Sub(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Sub(Const(n1),e2'),st')
| Sub(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Sub(e1',e2),st')
| Mul(Const(n1),Const(n2)) -> (Const(n1*n2),st)
| Mul(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Mul(Const(n1),e2'),st')
| Mul(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Mul(e1',e2),st')
| Eq(Const(n1),Const(n2)) -> if n1=n2 then (True,st) else (False,st)
| Eq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Eq(Const(n1),e2'),st')
| Eq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Eq(e1',e2),st')
| Leq(Const(n1),Const(n2)) -> if n1<=n2 then (True,st) else (False,st)
| Leq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Leq(Const(n1),e2'),st')
| Leq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Leq(e1',e2),st')
| Call(f, Const(n)) -> (
  match topenv st f with
  | IFun (x, c, er) ->
      let l = getloc st in
      let env' = bind (topenv st) x (IVar l) in
      let mem' = bind (getmem st) l n in
      let st' = (env'::getenv st, mem', l+1) in
      (CallExec (c, er), st')
  | _ -> raise (TypeError "Expected IFun, got IVar")
  )
| Call(f,e) -> let (e',st') = trace1_expr st e in (Call(f,e'),st')
| CallExec(c, er) -> (
  match trace1_cmd c with
  | _ -> failwith "Not yet implemented"
)
| _ -> failwith "Not yet implemented"
and trace1_cmd = function
| _ -> failwith "Not yet implemented"