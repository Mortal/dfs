(* should we introduce a cycle in the input? *)
let cycles = true

(* graph input *)
module NodeMap = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

type node = int

let nodes : node list = [2;3;4;1;5;6;7;8]

let edgeList (node : node) : node list =
  match node with
    | 1 ->
        if cycles then [3;4;7]
        else [4;7]
    | 2 -> [6;3]
    | 3 -> [5;8]
    | 4 -> []
    | 5 -> [1]
    | 6 -> [5;8]
    | 7 -> []
    | 8 -> [5;1]
    | _ -> raise Not_found

(*************)
(* DFS types *)

exception Cycle of (node NodeMap.t * node)

type dfs_info =
    { dfs_dtime   : int NodeMap.t;  (* discovery time *)
      dfs_ftime   : int NodeMap.t;  (* finishing time *)
      dfs_parents : node NodeMap.t; (* parent map *)
      dfs_time    : int }           (* most recent time *)

let dfs_empty_info =
  { dfs_dtime = NodeMap.empty;
    dfs_ftime = NodeMap.empty;
    dfs_parents = NodeMap.empty;
    dfs_time = 0 }

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
              let neighbours = edgeList v in (* recurse on neighbours of v *)
              let info = dfs_visit info neighbours v in
              let info = dfs_finish info v in
                info
        in
          dfs_visit info nodes u

let dfs_nocycles () =
  let rec visit info nodes =
    (* for each node u in G.V *)
    match nodes with
      | [] -> info
      | node :: nodes ->
          let info =
            let col = dfs_color info node in
              if col <> White then info
              else dfs_visit info [node] 0
          in visit info nodes
  in visit dfs_empty_info nodes



(*************)
(* main part *)
let () =
  try
    let _ = dfs_nocycles () in
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
