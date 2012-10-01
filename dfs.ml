(* should we introduce a cycle in the input? *)
let cycles = true

(* graph input *)
module NodeMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

type node = int

let edge_list_from_edges edges =
  let rec visit edges =
    match edges with
      | [] -> []
      | (u, v) :: _ ->
          let (cur, rest) =
            List.fold_left
              (fun (cur, rest) (u', v') ->
                 if u' = u then (v' :: cur, rest)
                 else (cur, (u', v') :: rest))
              ([], []) edges
          in
            (u, cur) :: visit rest
  in
  let edgeList = visit edges in
  let rec find u edgeList =
    match edgeList with
      | (u', vs) :: edgeList ->
          if u' = u then vs
          else find u edgeList
      | [] -> []
  in
    fun u -> find u edgeList


(*************)
(* DFS types *)

exception Cycle of (node NodeMap.t * node)

type dfs_info =
  { dfs_dtime    : int NodeMap.t;  (* discovery time *)
    dfs_ftime    : int NodeMap.t;  (* finishing time *)
    dfs_parents  : node NodeMap.t; (* parent map *)
    dfs_time     : int;            (* most recent time *)
    dfs_edgeList : node -> node list }

let dfs_init_info edgeList =
  { dfs_dtime    = NodeMap.empty;
    dfs_ftime    = NodeMap.empty;
    dfs_parents  = NodeMap.empty;
    dfs_time     = 0;
    dfs_edgeList = edgeList }

type dfs_color_type =
  | White    (* not yet discovered *)
  | Gray     (* on recursion stack *)
  | Black    (* already visited *)

(***************)
(* DFS helpers *)
let dfs_color info node =
  if not (NodeMap.mem node info.dfs_dtime)
  then White
  else
    if not (NodeMap.mem node info.dfs_ftime)
    then Gray
    else Black

(* field setters for all dfs_info fields *)

(* increment time *)
let dfs_inc_time info =
  { info with dfs_time = info.dfs_time + 1 }

(* increment time, set discover time for node, mark node Gray *)
let dfs_discover info node =
  let info = dfs_inc_time info in
  let dtime = NodeMap.add node info.dfs_time info.dfs_dtime in
    { info with dfs_dtime = dtime }

(* increment time, set finishing time for node, mark node Black *)
let dfs_finish info node =
  let info = dfs_inc_time info in
  let ftime = NodeMap.add node info.dfs_time info.dfs_ftime in
    { info with dfs_ftime = ftime }

(* add parent mapping *)
let dfs_add_parent info node parent =
  let parents = NodeMap.add node parent info.dfs_parents in
    { info with dfs_parents = parents }

(**********************)
(* DFS implementation *)

(* from node `u`, visit all neighbours `nodes` *)
let rec dfs_visit info nodes u =
  match nodes with
    | [] -> info
    | v :: nodes ->
        let info =
          (* for each v in G.adj[u] *)
          let col = dfs_color info v in
            if col = Black (* already visited, don't recurse *)
            then info
            else let info = dfs_add_parent info v u in
            if col = Gray (* on recursion stack -> cycle found *)
            then raise (Cycle (info.dfs_parents, v))
            else
              (* explore edge (u, v) *)
              let info = dfs_discover info v in
              let neighbours = info.dfs_edgeList v in (* recurse on neighbours of v *)
              let info = dfs_visit info neighbours v in
              let info = dfs_finish info v in
                info
        in
          dfs_visit info nodes u

let dfs_nocycles nodes edgeList =
  dfs_visit (dfs_init_info edgeList) nodes 0



(*************)
(* main part *)

let check_input nodes edges =
  let uses_node u (v, w) =
    (u = v) || (u = w)
  in
  let rec check_nodes nodes =
    match nodes with
      | [] -> ()
      | v :: nodes ->
          let () =
            if not (List.exists (uses_node v) edges) then
              print_endline ("Warning: Node " ^ (string_of_int v) ^ " is not connected to anything")
          in
            check_nodes nodes
  in
  let assert_node v nodes =
    if not (List.mem v nodes) then
      let () = print_endline ("Warning: Node " ^ (string_of_int v) ^ " referenced in edge not in node list")
      in v :: nodes
    else nodes
  in
  let check_edges all_edges nodes =
    let rec visit edges nodes =
      match edges with
        | [] -> (nodes, all_edges)
        | (u, v) :: edges ->
            let nodes = assert_node u nodes in
            let nodes = assert_node v nodes in
              visit edges nodes
    in
      visit all_edges nodes
  in
  let (nodes, edges) = check_edges edges nodes in
  let () = check_nodes nodes in
    (nodes, edges)

let () =
  try
    let chan = stdin in
    let lexbuf = Lexing.from_channel chan in
    let ast = Parser.goal Lexer.token lexbuf in
    let () = close_in chan in
    let nodes = Ast.get_nodes ast in
    let edges = Ast.get_edges ast in
    let (nodes, edges) = check_input nodes edges in
    let edgeList = edge_list_from_edges edges in
    let _ = dfs_nocycles nodes edgeList in
      print_endline "found no cycles"
  with
    | Cycle (parents, node) ->
        let rec print cur =
          let parent = NodeMap.find cur parents in
          let () =
            if parent <> node
            then print parent
            else print_string (string_of_int parent)
          in
            print_string (" -> " ^ (string_of_int cur))
        in
          print_endline "found cycles";
          print node;
          print_newline ()
