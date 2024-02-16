# json-parser-ml
Ocaml JSON parser

```ocaml

open Json_lib.Json_codec.Codec
open Json_lib.Json_parser.Parser

type person =
  {name: string; age: int}

(* optional converter *)
module Person_conv = struct 
  type t = person
  type fields = (string, int) field2
  
  let from_json ({fd1 = name; fd2 = age}: fields): t =
    {name = name; age = age}
  
  let to_json ({name = name; age = age}: person) : fields =
    {fd1 = name; fd2 = age}
end

let () =
    let json_str = "{\"name\": \"Jonh Do\", \"age\": 38 }" in
    let empty = {name = ""; age = 0} in
    let node = parse json_str in

    (* parse json to Record*)
    let p =
        (node, empty) 
            |> map (field_str "name" "") (fun v rd -> {rd with name = v})
            |> map (field_int "age" 0) (fun v rd -> {rd with age = v})      
            |> data
    in
    print_string (">> name = " ^ p.name ^ ", age = " ^ (string_of_int p.age) ^ "\n");

    (* or get a generic Record *)
    let result: (string, int) fields = 
        node |> map2 (field_str "name" "") (field_int "age" 0) |> 
    in    
        print_string (">> name = " ^ result.fd1 ^ ", age = " ^ (string_of_int result.fd2) ^ "\n");

    (* and convert to person ADT *)
    let person2 = 
        result |> from_json (module Person_conv) 
    in
    print_string ("<< name = " ^ person2.name ^ ", age = " ^ (string_of_int person2.age) ^ "\n");


```