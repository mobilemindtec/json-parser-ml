(* https://dev.to/vit0rr/building-a-json-parser-from-scratch-with-js-3180 *)
open Json_lib.Json_codec.Codec
open Json_lib.Json_parser.Parser


type person =
  {name: string; age: int}

module Person_conv = struct 
  type t = person
  type fields = (string, int) field2
  
  let from_json ({fd1 = name; fd2 = age}: fields): t =
    {name = name; age = age}
  
  let to_json ({name = name; age = age}: person) : fields =
    {fd1 = name; fd2 = age}
end    

let json =
  "{\"id\": 1, \"age\": 38, \"name\": \"ricardo\", \"list\": [1, 2, 3], \"list\": [{\"x\": \
   1}, {\"x\": 2}], \"other\": {\"value\": 5.2, \"enabled\": true}, \"empty\": \
   []}"

let () =
  let tokens = tokenize (json |> String.to_seq |> List.of_seq) in
  let node = parse_tokens tokens in
  print_ast node;
  let _ = print_string "\n" in

  let perrson =
    let empty = {name = ""; age = 0} in
    (node, empty) 
      |> map (field_str "name" "") (fun v rd -> {rd with name = v})
      |> map (field_int "age" 0) (fun v rd -> {rd with age = v})      
      |> data
  in
  print_string ("<< name = " ^ perrson.name ^ ", age = " ^ (string_of_int perrson.age) ^ "\n");
  let vals = 
    let result: (string, int) field2 = node |> map2 (field_str "name" "") (field_int "age" 0)  in
    result |> from_json (module Person_conv)    
  in
  print_string (">> name = " ^ vals.name ^ ", age = " ^ (string_of_int vals.age) ^ "\n");