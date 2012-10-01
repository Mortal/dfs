exception Syntax_error
exception Internal_error of string

type node = int

type decl =
  | NodeRange of (node * node)
  | Path of node list

let rec get_nodes decls =
  match decls with
    | [] -> []
    | NodeRange (n1, n2) :: decls ->
        range n1 n2 decls
    | Path _ :: decls -> get_nodes decls

and range n1 n2 decls =
  if n1 = n2 then n1 :: (get_nodes decls)
  else if n1 > n2 then raise Syntax_error
  else n1 :: (range (n1+1) n2 decls)

let rec get_edges decls =
  let rec parse_path vs decls =
    match vs with
      | u :: v :: vs -> (u, v) :: (parse_path (v :: vs) decls)
      | [v] -> get_edges decls
      | [] -> raise (Internal_error "Empty path")
  in
    match decls with
      | [] -> []
      | NodeRange _ :: decls -> get_edges decls
      | Path vs :: decls -> parse_path vs decls
