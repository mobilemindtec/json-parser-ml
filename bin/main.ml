(* https://dev.to/vit0rr/building-a-json-parser-from-scratch-with-js-3180 *)
open Json_lib.Json_codec.Codec
open Json_lib.Json_parser.Parser


let json =
  "{\"id\": 1, \"name\": \"ricardo\", \"list\": [1, 2, 3], \"list\": [{\"x\": \
   1}, {\"x\": 2}], \"other\": {\"value\": 5.2, \"enabled\": true}, \"empty\": \
   []}"

let () =
  let tokens = tokenize (json |> String.to_seq |> List.of_seq) in
  let node_ast = parse_tokens tokens in
  print_ast node_ast;
  let _ = print_string "\n" in


  let vals = 
    node_ast |> map2 (field_str "name" "") (field_int "id" 0) 
  in
  print_string (">> name = " ^ vals.fd1 ^ ", id = " ^ (string_of_int vals.fd2) ^ "\n");


