open Json_parser.Parser

module type Json_field_conv = sig

  type t
  val conv: ast_value -> t option  
end

module type Json_field = sig
  type t
  val field_opt: ast_prop list -> string -> t option
  
  val field: ast_prop list -> string -> t -> t
end 

module Make_json_field(Conv: Json_field_conv): Json_field with type t = Conv.t = struct
  
  type t = Conv.t
  
  let field_opt (props : ast_prop list) (field_name: string): t option =
    let p =
      List.find_opt (fun { name; _ } -> String.compare name field_name == 0) props
    in
    match p with
    | Some { value; _ } -> (
        match value with
        | NullLiteral -> None
        | _ -> Conv.conv value)
    | None -> None
    

  let field (props : ast_prop list) (field_name: string) (def_val: t): t =
    match field_opt props field_name with
    | Some v -> v
    | None -> def_val

end 

module Json_field_conv_str = struct
  type t = string  
  let conv value =
    match value with
    | StringLiteral i -> Some i
    | _ -> None 
end

module Json_field_conv_int = struct
  type t = int  
  let conv value =
    match value with
    | IntegerLiteral i -> Some i
    | _ -> None 
end

module Json_field_conv_float = struct
  type t = float  
  let conv value =
    match value with
    | NumberLiteral i -> Some i
    | _ -> None 
end

module Json_field_conv_bool = struct
  type t = bool
  let conv value =
    match value with
    | BooleanLiteral i -> Some i
    | _ -> None 
end

module Json_field_str = Make_json_field(Json_field_conv_str)
module Json_field_int = Make_json_field(Json_field_conv_int)
module Json_field_float = Make_json_field(Json_field_conv_float)
module Json_field_bool = Make_json_field(Json_field_conv_bool)

module Codec = struct

  let field_opt (type a) (module JsonField: Json_field with type t = a) (field_name: string) (ast: ast_value): a option =
    match ast with
    | Object props -> JsonField.field_opt props field_name
    | _ -> None

  let field (type a) ((module JsonField: Json_field with type t = a) as instance) (field_name: string) (def_value: a) (ast: ast_value): a =
    match field_opt instance field_name ast with
    | Some v -> v
    | None -> def_value 
    
  let field_opt_str (field_name: string) (ast: ast_value): string option =
    field_opt (module Json_field_str) field_name ast
    
  let field_opt_int (field_name: string) (ast: ast_value): int option =
    field_opt (module Json_field_int) field_name ast

  let field_opt_int (field_name: string) (ast: ast_value): int option =
    field_opt (module Json_field_int) field_name ast

  let field_opt_float (field_name: string) (ast: ast_value): float option =
    field_opt (module Json_field_float) field_name ast

  let field_opt_bool (field_name: string) (ast: ast_value): bool option =
    field_opt (module Json_field_bool) field_name ast

    let field_str (field_name: string) (v: string) (ast: ast_value): string =
    field (module Json_field_str) field_name v ast
    
  let field_int (field_name: string) (v: int) (ast: ast_value): int =
    field (module Json_field_int) field_name v ast

  let field_float (field_name: string) (v: float) (ast: ast_value): float =
    field (module Json_field_float) field_name v ast

  let field_bool (field_name: string) (v: bool) (ast: ast_value): bool =
    field (module Json_field_bool) field_name v ast
  
  type 'a conv = ast_value -> 'a
  type 'x1 field1 = { fd1 : 'x1 }
  type ('x1, 'x2) field2 = { fd1 : 'x1; fd2: 'x2 }

  let map 
    (f1: 'x1 conv) 
    (ast: ast_value): 'x1 field1 =
    {fd1 = f1 ast}

  let map2 
    (f1: 'x1 conv) 
    (f2: 'x2 conv) 
    (ast: ast_value): ('x1, 'x2) field2 =
    {fd1 = f1 ast; fd2 = f2 ast}
  
  let map3 
    (f1: 'x1 conv) 
    (f2: 'x2 conv) 
    (f3: 'x3 conv) 
    (ast: ast_value): 'x1 * 'x2 *'x3 =
    f1 ast, f2 ast, f3 ast

  let map4 
    (f1: 'x1 conv) 
    (f2: 'x2 conv) 
    (f3: 'x3 conv)
    (f4: 'x4 conv) 
    (ast: ast_value): 'x1 * 'x2 * 'x3 * 'x4 =
    f1 ast, f2 ast, f3 ast, f4 ast

  let map5 
    (f1: 'x1 conv) 
    (f2: 'x2 conv) 
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv) 
    (ast: ast_value): 'x1 * 'x2 * 'x3 * 'x4 * 'x5 =
    f1 ast, f2 ast, f3 ast, f4 ast, f5 ast

  let map6 
    (f1: 'x1 conv) 
    (f2: 'x2 conv) 
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv) 
    (f6: 'x6 conv) 
    (ast: ast_value): 'x1 * 'x2 * 'x3 * 'x4 * 'x5 * 'x6 =
    f1 ast, f2 ast, f3 ast, f4 ast, f5 ast, f6 ast
  
  let map7 
    (f1: 'x1 conv) 
    (f2: 'x2 conv) 
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv) 
    (f6: 'x6 conv) 
    (f7: 'x7 conv) 
    (ast: ast_value): 'x1 * 'x2 * 'x3 * 'x4 * 'x5 * 'x6 * 'x7 =
    f1 ast, f2 ast, f3 ast, f4 ast, f5 ast, f6 ast, f7 ast

end

