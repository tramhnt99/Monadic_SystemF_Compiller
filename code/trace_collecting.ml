(*
Trace Collecting Semantics for Monadic Interpreter
Year 4 Capstone Project
Tram Hoang
 *)

module TraceCollectingSemanMonad = struct
  open SystemF_sig.SystemF0Signature
  open Monad_systemF_sig.MonadSystemFSignature
  open Datastructures.LinkedGraph

  (*
    function result - 'a option
    (environment, log list) graph - trace semantics graph
                  nodes - environment (states)
                  edges - log list (function calls that lead to said states)
    log list - function calls that haven't lead to updated states
   *)

  type 'a t = 'a option * (environment, log list) graph * log list
  let empty = mk_graph

  (* let return x name prev_state curr_state = 
   *   if prev_state = curr_state then *)
      
      
            
   
end
