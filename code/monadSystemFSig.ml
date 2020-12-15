(*
Monadic System F Interpreter - Signature
Year 4 Capstone Project
Tram Hoang
*)

open SystemFSig.SystemF0Signature

module type MonadAbstractSig = 
  sig
    type 'a monad
    val return : 'a -> log -> environment option -> 'a monad
    val (>>=) : 'a monad -> ('a -> 'b monad) -> 'b monad
    val return_error: string -> environment -> 'a monad
    val get_result: 'a monad -> 'a option
  end

(*Just logs the function calls*)
module LogMonad : MonadAbstractSig = 
  struct
    
    type 'a monad = 'a option * log list

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

  end

(*Log function call with it's states *)
module SemanticsMonad : MonadAbstractSig = 
  struct
    type 'a monad = 'a option * (log * environment) list

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
  end


