open Ast
open Types
    
let apply ( st : state ) ( x : ide ) =
  match topenv st x with
  | IVar l
  | BVar l -> getmem st l

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

let rec eval_expr st = function
    True -> Bool true
  | False -> Bool false
  | Var x -> apply st x
  | Const n -> Int n
  | Not(e) -> (match eval_expr st e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
    )
  | Or(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 + n2)
      | _ -> raise (TypeError "Add")
    )    
  | Sub(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) when n1>=n2 -> Int(n1 - n2)
      | _ -> raise (TypeError "Sub")
    )
  | Mul(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Int(n1 * n2)
      | _ -> raise (TypeError "Add")
    )        
  | Eq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
    )    
  | Leq(e1,e2) -> (match (eval_expr st e1,eval_expr st e2)  with
        (Int n1,Int n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
    )


(**************************************************************)
(*           Small-step semantics of commands                 *)
(**************************************************************)

let bottom_env = fun x -> raise (UnboundVar x)
let bottom_mem = fun x -> raise (UnboundLoc x)

let bind f x v = fun y -> if y = x then v else f y

let rec eval_decl (e,l : env * loc) = function
  | EmptyDecl -> (e,l)
  | IntVar (x,d) -> let e' = bind e x (IVar l) in eval_decl (e', l+1) d
  | BoolVar (x,d) -> let e' = bind e x (BVar l) in eval_decl (e', l+1) d

let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd(cmd, st) -> (
    match cmd with
    | Skip -> St st
    | Assign (x, e) -> (
      match (eval_expr st e, topenv st x) with
      | (Bool b, BVar l) -> St (getenv st, bind (getmem st) l (Bool b), (getloc st) + 1)
      | (Int v, IVar l)  -> St (getenv st, bind (getmem st) l (Int v),  (getloc st) + 1)
      | _ -> raise (TypeError "Assign")
    )
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
    | Decl(d, c) -> 
      let (e, l) = eval_decl (topenv st, getloc st) d in
      let st' = (e :: (getenv st), getmem st, l) in
        Cmd(Block(c), st')
    | Block(c) -> (
      match trace1 (Cmd(c, st)) with
      | St st -> St (popenv st, getmem st, getloc st)
      | Cmd(c', st') -> Cmd(Block(c'), st')
    )
  )

(**********************************************************************
 trace_rec : int-> conf -> conf list

 Usage: trace_rec n t performs n steps of the small-step semantics

**********************************************************************)

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

(**********************************************************************
 trace : int -> cmd -> conf list

 Usage: trace n c performs n steps of the small-step semantics
**********************************************************************)

let trace n c = trace_rec n (Cmd(c,([bottom_env], bottom_mem, 0)))