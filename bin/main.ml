(* https://dev.to/vit0rr/building-a-json-parser-from-scratch-with-js-3180 *)
open Json_lib.Json_codec.Codec
open Json_lib.Json_parser.Parser


type person =
  {name: string; age: int}

module Person_conv = struct 
  type t = person
  type fds = (string, int) field2
  
  let from_json ({fd1 = name; fd2 = age}: fds): t =
    {name = name; age = age}
  
  let to_json ({name = name; age = age}: person) : fds =
    {fd1 = name; fd2 = age}

end

let json =
  "{\"id\": 1, \"age\": 38, \"name\": \"ricardo\", \"list\": [1, 2, 3], \"list\": [{\"x\": \
   1}, {\"x\": 2}], \"other\": {\"value\": 5.2, \"enabled\": true}, \"empty\": \
   []}"

let () =
  let tokens = tokenize (json |> String.to_seq |> List.of_seq) in
  let node_ast = parse_tokens tokens in
  print_ast node_ast;
  let _ = print_string "\n" in

  let vals = 
    node_ast |> map2 (field_str "name" "") (field_int "age" 0) |> from_json (module Person_conv)    
  in
  print_string (">> name = " ^ vals.name ^ ", age = " ^ (string_of_int vals.age) ^ "\n");