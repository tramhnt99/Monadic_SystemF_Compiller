(* 
Graph Datastructure for Trace Collecting Semantics
Year 4 Capstone Project
Tram Hoang
*)


(* 
Building a Linked Graph for lower complexity of adding edges and 
finding successors.

Nodes - states or the environments of function calls
Edges - function calls that lead to certain states

Node and Edge identifiers are of type int.

*)

module LinkedGraph = struct
  
  type 'a node = {
      id : int;
      value : 'a ref;
      next : int list ref;
      prev : int list ref
    }

  let get_value n = !(n.value)
  let get_next n = !(n.next)
  let get_prev n = !(n.prev)

  let add_prev node src =
    let prev' = get_prev node |>
                  List.filter (fun n -> n <> src) in
    node.prev := src :: prev'

  let add_next node dst =
    let next' = get_next node |>
                  List.filter (fun n -> n <> dst) in
    node.next := dst :: next'


  (*IntSet and IntHash types for creating int based Sets and HashTables*)
  module IntHash =
    struct
      type t = int
      let equal i j = i=j
      let hash i = i land max_int
    end
  module IntIntOptHash =
    struct
      type t = int * int option
      let equal (i1, i2) (j1, j2) = i1 = j1 && i2 = j2
      let hash (i1, i2) = 
        match i2 with
        | None -> (i1 land max_int)
        | Some i -> (i1 land max_int) land (i land max_int)
    end

    

  (* Set of Nodes and Edges modules *)
  module NodeSet = Set.Make(
                       struct
                         let compare = Stdlib.compare
                         type t = int
                       end
                     )
  module EdgeSet = Set.Make(
                       struct
                         (* might update the compare method for edges *)
                         let compare = Stdlib.compare
                         type t = int * int option
                       end
                     )

  (* Hash Tables of Nodes and Edges Identifiers to actual Nodes and Edges *)
  module NodeTable = Hashtbl.Make(IntHash)
  module EdgeTable = Hashtbl.Make(IntIntOptHash)

  (*Type constructor for an 'a, 'b graph 
    next_node_id - next ref number to be id
    nodes - Set of all nodes
    id_node_map - A map from Id's of nodes to the node itself
    vnode_id_map - A map from values of nodes to the id of the node
    edges - set edges of the graph
    edge_map - A map from edge id (src,dst) (where src and dst are node ids) to the     value of the edge
   *)
  type ('a, 'b) graph = {
      next_node_id: int ref;
      nodes: NodeSet.t ref;
      id_node_map: 'a node NodeTable.t;
      vnode_id_map: ('a, int) Hashtbl.t;

      edges: EdgeSet.t ref;
      edge_map: 'b EdgeTable.t;
    }

  (*Size of the graph, found by current next id number*)
  let n_size g = !(g.next_node_id)

  let e_size g = EdgeSet.cardinal !(g.edges)
  let get_nodes g = NodeSet.elements !(g.nodes)
  let get_node g i = NodeTable.find g.id_node_map i
                       
  let get_succ g n : int list =
    let node = get_node g n in
    get_next node

  let get_prev g n : int list =
    let node = get_node g n in
    get_prev node

  (*Check if node is in graph by its id*)
  let id_node_in_graph g id : bool =
    let nodes = g.nodes in
    try 
      let _ = NodeSet.find id !nodes in
      true
    with 
      _ -> false

  (*Check if the node is in the graph by its value*)
  let vnode_in_graph g value : bool =
    try
      let id = Hashtbl.find g.vnode_id_map value in
      id_node_in_graph g id (*since this function only checks the set*)
    with
      _ -> false

  (*Find the edge in graph by id of the nodes*)
  let edge_in_graph g (src: int) (dst: int option) : bool =
    let edges = g.edges in
    try
      let _ = EdgeSet.find (src, dst) !edges in
      true
    with
      _ -> false

  let value_of_edge g src dst = 
    assert (edge_in_graph g src dst);
    try 
      Some (EdgeTable.find g.edge_map (src, dst))
    with 
      _ -> None

  let mk_graph _ = {
      next_node_id = ref 0;
      nodes = ref NodeSet.empty;
      id_node_map = NodeTable.create 5; (*5 is the starting size*)
      vnode_id_map = Hashtbl.create 5;
      edges = ref EdgeSet.empty;
      edge_map = EdgeTable.create 10;
    }

  let add_node g v : unit =
    let new_id = !(g.next_node_id) in
    g.next_node_id := !(g.next_node_id) + 1;
    let node = {
        id = new_id;
        value = ref v;
        next = ref [];
        prev = ref [];
      } in
    g.nodes := NodeSet.add new_id !(g.nodes);
    NodeTable.add g.id_node_map new_id node;
    Hashtbl.add g.vnode_id_map v new_id
    
  (*Add edge with it's corresponding value*)
  (*Dst to an edge is option int - because we want edges that point outwards
    before we know the state it's going to reach
   *)
  let add_edge g (src: 'a) (dst: 'a option) value : unit =
    assert (vnode_in_graph g src);
    let (src_id, dst_id) = 
      let s_id = Hashtbl.find g.vnode_id_map src in
      match dst with
      | None -> 
         (*Update the edge map*)
         EdgeTable.add g.edge_map (s_id, None) value;
         (*We don't update the nodes cause there's no dst id*)
         (s_id, None)
      | Some dst_value -> 
         assert (vnode_in_graph g dst_value);
         let d_id = Hashtbl.find g.vnode_id_map dst_value in
         (*Update the nodes*)
         let src_node = NodeTable.find g.id_node_map s_id in
         let dst_node = NodeTable.find g.id_node_map d_id in
         add_prev dst_node s_id;
         add_next src_node d_id;
         (*Update the edge map*)
         EdgeTable.add g.edge_map (s_id, Some d_id) value;
         (s_id, Some d_id)
    in
    (*Update the set of edges*)
    g.edges := EdgeSet.add (src_id, dst_id) !(g.edges);
         
    



  (*Combine the graph g2 into graph g1*)
  (* let combine_graphs g1 g2 : ('a, 'b) graph =
   *   
   *   NodeSet.iter (fun node -> 
   *       if id_node_in_graph g1 node then (\* just update the edges *\)
   *         
   *         
   *     ) g2 *)
    
    

end
