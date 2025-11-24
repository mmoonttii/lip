open Ast
    
type loc = int

type envval = IVar of loc | IFun of ide * cmd * expr
type memval = int

type env = ide -> envval
type mem = loc -> memval

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type state = env list * mem * loc

let topenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv (el,_,_) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv (el,_,_) = el
let getmem (_,m,_) = m
let getloc (_,_,l) = l
  
type conf = St of state | Cmd of cmd * state


(** [pushenv st env] is the environment stack of [st] with [env] on top. *)
let pushenv st env = env :: (getenv st)
