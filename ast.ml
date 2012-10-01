exception Syntax_error

type node = int

type decl =
  | NodeRange of (node * node)
  | Edge of (node * node)

let rec get_nodes decls =
  match decls with
    | [] -> []
    | NodeRange (n1, n2) :: decls ->
        range n1 n2 decls
    | Edge _ :: decls -> get_nodes decls

and range n1 n2 decls =
  if n1 = n2 then n1 :: (get_nodes decls)
  else if n1 > n2 then raise Syntax_error
  else n1 :: (range (n1+1) n2 decls)

let rec get_edges decls =
  match decls with
    | [] -> []
    | NodeRange _ :: decls -> get_edges decls
    | Edge (n1, n2) :: decls -> (n1, n2) :: (get_edges decls)
