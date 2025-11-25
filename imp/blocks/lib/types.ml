open Ast
    
type loc = int

type envval = BVar of loc | IVar of loc
type memval = Bool of bool | Int of int

type env = ide -> envval
type mem = loc -> memval

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type state = env list * mem * loc

let topenv (el,_,_ : state) : env = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_ : state) : env list = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_ : state) : env list= el
let getmem (_,m,_ : state) : mem = m
let getloc (_,_,l : state) : loc = l
  
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc
exception NoRuleApplies