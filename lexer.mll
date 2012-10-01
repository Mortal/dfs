{
 open Parser
 exception Eof
}
rule token = parse
  [' ' '\t']  { token lexbuf }
| ['\n' '\r'] { NEWLINE }
| ['0'-'9']* as lxm { NODE lxm }
| ".." { RANGE }
| "," { COMMA }
| "->" { ARROW }
| eof { EOF }
| _ as lxm { Printf.printf "Illegal character %c" lxm; failwith "Bad input" }
