(*
Trace Collecting Semantics for Monadic Interpreter
Year 4 Capstone Project
Tram Hoang
 *)

module TraceCollectingSemanMonad = struct
  (* open SystemFSig.SystemF0Signature
   * open MonadSystemFSig.MonadSystemFSignature
   * open Datastructures.LinkedGraph *)

  (*
    function result - 'a option
    (environment, log list) graph - trace semantics graph
                  nodes - environment (states)
                  edges - log list (function calls that lead to said states)
   *)

  (* type 'a t = 'a option * (environment, log list) graph
   * let empty = mk_graph *)

  (* Combine the graph g2 into graph g1 *)
  (*Note that although ids are unique in a graph, between graphs they are not*)
            
  (* let combine_graphs g1 g2 : (environment, log list) graph =
   *   (\*Since nodes are ordered, we just iter through them
   *     + States are updated monotonically, so there won't be loops
   *     + Or there shouldn't be combination to a past state node that's resolved
   *    *\)
   *   let g2_min = NodeTable.find g2.id_node_map 0 in
   *   if vnode_in_graph g1 g2_min.value then
   *     begin
   *     (\*we're just attaching the graph g2 to the appropriate node in g1*\)
   *       let curr_g2_node = ref g2_min in
   *       while !((!curr_g2_node).next) != [] do
   *         let c_node = !curr_g2_node in
   *         let g1_node_id = Hashtbl.find g1.vnode_id_map !(c_node.value) in
   *         let g1_node = NodeTable.find g1.id_node_map g1_node_id in
   *         let to_add_next = 
   *           List.filter (fun )
   *       
   *       
   *       done;
   *     end
   *   g1 *)
                                                                           

  (* let return x name prev_state curr_state = 
   *   if prev_state = curr_state then *)
      
      
            
   
end
