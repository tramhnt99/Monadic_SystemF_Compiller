(*
Monadic System F Interpreter - Signature
Year 4 Capstone Project
Tram Hoang
*)

open SystemFSig.SystemF0Signature
open Utils
(* open Core_kernel
 * open Result.Let_syntax *)

module type MonadAbstractSig = 
  sig
    type 'a monad
    type m
    val return : 'a -> log -> environment option -> 'a monad
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

    let return_error s state = 
      let _ = state in
      None, [Error s]

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
      (Some x, [(name, Option.get state)])

    let (>>=) (m: 'a monad)
      (f: 'a -> 'b monad) : 'b monad =
      match m with
      | (None, l) -> None, l
      | (Some e, l) ->
         let res, new_l = f e in
         res, new_l @ l

    let return_error s state = None, [(Error s, state)]
                             
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
