(*
Monadic System F Interpreter - Signature
Year 4 Capstone Project
Tram Hoang
*)

open SystemFSig.SystemF0Signature
open Utils

module type MonadAbstractSig = 
sig
  type 'a monad
  type m
  val return : 'a -> log -> environment -> 'a monad
  val (>>=) : 'a monad -> ('a -> 'b monad) -> 'b monad
  val return_error: string -> environment -> 'a monad
  val get_result: 'a monad -> 'a option
  val get_monad: 'a monad -> m
  val monad_to_string: m -> string
  val construct_monad: 'a option -> m -> 'a monad
end

(*Just logs the function calls*)
module LogMonad : MonadAbstractSig = 
struct

  type 'a monad = 'a option * log list
  type m = log list

  let return (x: 'a) name state : 'a monad =
    let _ = state in
    (Some x, [name])

  let (>>=) (m: 'a monad)
      (f: 'a -> 'b monad) : 'b monad =
    match m with
    | (None, l) -> (None, l)
    | (Some e, l) -> 
      let res, new_l = f e in
      res, new_l @ l

  let return_error s state: 'a monad = 
    let _ = state in
    None, [ErrorLog s]

  let get_result m = fst m 
  let get_monad (m: 'a monad): 'b = snd m
  let construct_monad first second = first, second

  let monad_to_string (m: m) = 
    let logs =
      List.fold_left (fun res l ->
          String.concat " " [(string_of_log l); ";"; res]
        ) "" m
    in 
    "[ " ^ logs ^ " ]"

end

(*Log function call with it's states *)
module SemanticsMonad : MonadAbstractSig = 
struct
  type 'a monad = 'a option * (log * environment) list
  type m = (log * environment) list

  let return (x: 'a) name state : 'a monad =
    (Some x, [(name, state)])

  let (>>=) (m: 'a monad)
      (f: 'a -> 'b monad) : 'b monad =
    match m with
    | (None, l) -> None, l
    | (Some e, l) ->
      let res, new_l = f e in
      res, new_l @ l

  let return_error s state: 'a monad = None, [(ErrorLog s, state)]

  let get_result m = fst m
  let get_monad (m: 'a monad) : 'b = snd m
  let construct_monad first second = (first, second)

  let monad_to_string (m: m) = 
    let logs = 
      List.fold_left (fun res (l, env) ->
          String.concat " " 
            [string_of_log l; string_of_env env; ";"; res]
        ) "" m
    in
    "[ " ^ logs ^ " ]"
end

(*CPS Monad*)
module CPSLogMonad = struct
  (* Continuation k takes on result and log *)
  type ('a, 'b) monad = (('a, String.t) result -> 'b) -> 'b
  let return x (k:('a, String.t) result -> 'b) = k (Ok x)

  let (>>=) x f k = 
    let k' r = 
      match r with
      | Ok z -> (f z) k   
      | Error _ as x' -> k x' in
    x k'

  (* let (>>=) (x: ('a, log list -> 'b) monad) 
    (f: 'a -> (('a, String.t) result -> log list -> 'b) -> log list -> 'b)
    (k: ('a, String.t) result -> log list -> 'b) 
    (current_log: log list): ('c, 'b) monad = 
    let k' r =
      match r with
      | Ok z -> (f z) k
      | Error _ as x' -> k x'
    in
    x k' *)

  (*NOTE: you can't apply k to a log list because that is not handled in the function eval *)

  let return_error s k = 
    k (Error s)
    
  let grow_collection thunk log k current_log =
      thunk () k (current_log @ log)

end


(*CPS Monad Log*)
(* module CPSLogMonad = 
 * struct
 *   (\*for now just log `log`, can add environment later *\)
 *   type ('a, 'b) monad = ((('a, String.t) result * log list -> 'b) -> 'b) 
 *   type m = log list
 * 
 *   let return x k : ('a, 'b) monad = k @@ Ok x
 *       
 *   let (>>=) (m: ('a, 'b) monad) f : ('c, 'b) monad = fun k ->
 *     let k' r = match r with
 *       | Ok x, l -> 
 *         
 *     
 *                                                 
 *   (\* let (>>=) (m: ('a, 'b) monad) f : ('c, 'b) monad =
 *    *   fun k ->
 *    *   let cont = fst m in
 *    *   let logging = snd m in
 *    *   let k' r =
 *    *     match r with
 *    *     | Ok z -> 
 *    *       let cont, new_l = f z in
 *    *       cont k, new_l @ logging
 *    *     | Error _ as x' -> k x', l
 *    *   in cont k' *\)
 * 
 *   let return_error s state: ('a, 'b) monad = Error s, [(ErrorLog s, state)]     
 *   
 *   let get_result m = fst m
 *   let get_monad (m: ('a, 'b) monad) : 'b = snd m
 *   let construct_monad first second = (first, second)
 *                                      
 *   let monad_to_string (m:m) = failwith "unimplemented"
 *       
 *   
 *   
 * end *)
