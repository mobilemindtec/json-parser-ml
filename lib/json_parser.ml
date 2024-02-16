
module type Parser_sig = sig
  
  type tokentype =
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | Colon
    | Comma
    | Str of string
    | Number of string
    | True
    | False
    | Null

  type ast_prop = 
    { name : string; value : ast_value }
  and ast_value =
    | StringLiteral of string
    | NumberLiteral of float
    | IntegerLiteral of int
    | BooleanLiteral of bool
    | NullLiteral
    | Array of ast_value list
    | Object of ast_prop list

    
  val tokenize: char list -> tokentype list

  val parse: string -> ast_value

  val parse_file_ext: string -> ast_value

  val parse_tokens: tokentype list -> ast_value

  val pritn_tokens: tokentype list -> unit

  val print_ast : ast_value -> unit

end 

module Parser : Parser_sig = struct  

    type tokentype =
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | Colon
    | Comma
    | Str of string
    | Number of string
    | True
    | False
    | Null

  type ast_prop = 
    { name : string; value : ast_value }
  and ast_value =
    | StringLiteral of string
    | NumberLiteral of float
    | IntegerLiteral of int
    | BooleanLiteral of bool
    | NullLiteral
    | Array of ast_value list
    | Object of ast_prop list  

    let mk_ast_prop name value = { name; value }
    
    let token_to_str token =
      match token with
      | LeftBrace -> "{"
      | RightBrace -> "}"
      | LeftBracket -> "["
      | RightBracket -> "]"
      | Colon -> ":"
      | Comma -> ","
      | Str s -> "S:" ^ s
      | Number s -> "N:" ^ s
      | True -> "B:true"
      | False -> "B:false"
      | Null -> "U:null"
    
    let char_lst_to_str char_list = String.of_seq (List.to_seq char_list)
        
    let rec find_string str acc =
      match str with
      | '"' :: xs -> (acc, xs)
      | x :: xs -> find_string xs (acc @ [ x ])
      | _ -> raise (Failure "premature end of string")
    
    let rec find_value str acc =
      match str with
      | ',' :: xs -> (Comma, acc, xs)
      | '}' :: xs -> (RightBrace, acc, xs)
      | ']' :: xs -> (RightBracket, acc, xs)
      | x :: xs -> find_value xs (acc @ [ x ])
      | _ -> raise (Failure "premature end of value")
    
    let select_value_type value =
      match value with
      | "true" -> True
      | "false" -> False
      | "null" -> Null
      | _ -> Number value
    
    let rec create_tokens str (acc : tokentype list) =
      match str with
      | ' ' :: xs -> create_tokens xs acc
      | '{' :: xs -> create_tokens xs (acc @ [ LeftBrace ])
      | '}' :: xs -> create_tokens xs (acc @ [ RightBrace ])
      | '[' :: xs -> create_tokens xs (acc @ [ LeftBracket ])
      | ']' :: xs -> create_tokens xs (acc @ [ RightBracket ])
      | ':' :: xs -> create_tokens xs (acc @ [ Colon ])
      | ',' :: xs -> create_tokens xs (acc @ [ Comma ])
      | '"' :: xs ->
          let value, rest = find_string xs [] in
          let str_val = char_lst_to_str value in
          create_tokens rest (acc @ [ Str str_val ])
      | x :: xs ->
          let next_token_typ, value, rest = find_value (x :: xs) [] in
          let str_val = char_lst_to_str value in
          let typ = select_value_type str_val in
          create_tokens rest (acc @ [ typ; next_token_typ ])
      | [] -> acc
    
    let tokenize str =
      match str with
      | '{' :: _ -> create_tokens str []
      | '[' :: _ -> create_tokens str []
      | '"' :: _ -> create_tokens str []
      | _ -> [ select_value_type (char_lst_to_str str) ]
    
    let rec parse_token_to_array tokens acc =
      match tokens with
      | RightBracket :: rest -> (rest, acc)
      | token :: rest ->
          let value, rst = parse_token_to_ast token rest in
          parse_token_to_array rst (acc @ [ value ])
      | [] -> ([], acc)
    
    and parse_token_to_ast token rest =
      match token with
      | Str s -> (StringLiteral s, rest)
      | Number s ->
          if String.contains s '.' then (NumberLiteral (Float.of_string s), rest)
          else (IntegerLiteral (int_of_string s), rest)
      | True -> (BooleanLiteral true, rest)
      | False -> (BooleanLiteral false, rest)
      | Null -> (NullLiteral, rest)
      | LeftBrace ->
          let props, rst = parse_token_to_properties rest [] in
          (Object props, rst)
      | LeftBracket ->
          let rst, arr = parse_token_to_array rest [] in
          (Array arr, rst)
      | Comma -> parse_token_to_ast (List.hd rest) (List.tl rest)
      | _ -> raise (Failure ("wrong token type " ^ token_to_str token))
    
    and parse_token_to_properties tokens props =
      match tokens with
      (*field:value::rest*)
      | Str fld :: _ :: v :: xs ->
          let literal, rst = parse_token_to_ast v xs in
          parse_token_to_properties rst (props @ [ mk_ast_prop fld literal ])
      | Comma :: xs -> parse_token_to_properties xs props
      | RightBrace :: rts -> (props, rts)
      | _ -> raise (Failure "wrong end prop type")
    
    and parse_tokens tokens =
      match tokens with
      | x :: xs ->
          let value, _ = parse_token_to_ast x xs in
          value
      | [] -> Object []

    let parse str = 
      str |> String.to_seq |> List.of_seq |> tokenize |> parse_tokens

    let parse_file_ext file_path =
      try
        let lines = In_channel.with_open_bin file_path In_channel.input_all in
        lines |> parse
      with e ->
        raise e        
    
    let ast_to_str node =
      match node with
      | StringLiteral v -> v
      | NumberLiteral f -> string_of_float f
      | IntegerLiteral f -> string_of_int f
      | BooleanLiteral b -> if b then "true" else "false"
      | NullLiteral -> "null"
      | _ -> "-"
    
    let rec print_ast node =
      match node with
      | Array values ->
          List.iter
            (fun v ->
              print_ast v;
              print_string ",")
            values
      | Object props ->
          List.iter
            (fun { name; value } ->
              print_string (name ^ "=");
              print_ast value;
              print_string ",")
            props
      | other -> print_string (ast_to_str other)

    let rec pritn_tokens tokens =
      match tokens with
      | x :: xs ->
          let str = token_to_str x in
          print_string str;
          pritn_tokens xs
      | [] -> print_string "\n"
  
end


