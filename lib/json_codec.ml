open Json_parser.Parser



module Codec = struct

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

  type ('x1) field1 = { fd1 : 'x1}
  type ('x1, 'x2) field2 = { fd1 : 'x1; fd2 : 'x2}
  type ('x1, 'x2, 'x3) field3 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3}
  type ('x1, 'x2, 'x3, 'x4) field4 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4}
  type ('x1, 'x2, 'x3, 'x4, 'x5) field5 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6) field6 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7) field7 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8) field8 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9) field9 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10) field10 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11) field11 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12) field12 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13) field13 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14) field14 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13; fd14 : 'x14}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15) field15 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13; fd14 : 'x14; fd15 : 'x15}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16) field16 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13; fd14 : 'x14; fd15 : 'x15; fd16 : 'x16}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17) field17 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13; fd14 : 'x14; fd15 : 'x15; fd16 : 'x16; fd17 : 'x17}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17, 'x18) field18 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13; fd14 : 'x14; fd15 : 'x15; fd16 : 'x16; fd17 : 'x17; fd18 : 'x18}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17, 'x18, 'x19) field19 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13; fd14 : 'x14; fd15 : 'x15; fd16 : 'x16; fd17 : 'x17; fd18 : 'x18; fd19 : 'x19}
  type ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17, 'x18, 'x19, 'x20) field20 = { fd1 : 'x1; fd2 : 'x2; fd3 : 'x3; fd4 : 'x4; fd5 : 'x5; fd6 : 'x6; fd7 : 'x7; fd8 : 'x8; fd9 : 'x9; fd10 : 'x10; fd11 : 'x11; fd12 : 'x12; fd13 : 'x13; fd14 : 'x14; fd15 : 'x15; fd16 : 'x16; fd17 : 'x17; fd18 : 'x18; fd19 : 'x19; fd20 : 'x20}

  let map1
    (f1: 'x1 conv)
    (ast: ast_value): ('x1) field1 =
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
    (ast: ast_value): ('x1, 'x2, 'x3) field3 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast}

  let map4
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4) field4 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast}

  let map5
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5) field5 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast}

  let map6
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6) field6 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast}

  let map7
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7) field7 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast}

  let map8
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8) field8 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast}

  let map9
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9) field9 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast}

  let map10
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10) field10 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast}

  let map11
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11) field11 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast}

  let map12
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12) field12 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast}

  let map13
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13) field13 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast}

  let map14
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (f14: 'x14 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14) field14 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast; fd14 = f14 ast}

  let map15
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (f14: 'x14 conv)
    (f15: 'x15 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15) field15 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast; fd14 = f14 ast; fd15 = f15 ast}

  let map16
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (f14: 'x14 conv)
    (f15: 'x15 conv)
    (f16: 'x16 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16) field16 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast; fd14 = f14 ast; fd15 = f15 ast; fd16 = f16 ast}

  let map17
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (f14: 'x14 conv)
    (f15: 'x15 conv)
    (f16: 'x16 conv)
    (f17: 'x17 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17) field17 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast; fd14 = f14 ast; fd15 = f15 ast; fd16 = f16 ast; fd17 = f17 ast}

  let map18
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (f14: 'x14 conv)
    (f15: 'x15 conv)
    (f16: 'x16 conv)
    (f17: 'x17 conv)
    (f18: 'x18 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17, 'x18) field18 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast; fd14 = f14 ast; fd15 = f15 ast; fd16 = f16 ast; fd17 = f17 ast; fd18 = f18 ast}

  let map19
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (f14: 'x14 conv)
    (f15: 'x15 conv)
    (f16: 'x16 conv)
    (f17: 'x17 conv)
    (f18: 'x18 conv)
    (f19: 'x19 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17, 'x18, 'x19) field19 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast; fd14 = f14 ast; fd15 = f15 ast; fd16 = f16 ast; fd17 = f17 ast; fd18 = f18 ast; fd19 = f19 ast}

  let map20
    (f1: 'x1 conv)
    (f2: 'x2 conv)
    (f3: 'x3 conv)
    (f4: 'x4 conv)
    (f5: 'x5 conv)
    (f6: 'x6 conv)
    (f7: 'x7 conv)
    (f8: 'x8 conv)
    (f9: 'x9 conv)
    (f10: 'x10 conv)
    (f11: 'x11 conv)
    (f12: 'x12 conv)
    (f13: 'x13 conv)
    (f14: 'x14 conv)
    (f15: 'x15 conv)
    (f16: 'x16 conv)
    (f17: 'x17 conv)
    (f18: 'x18 conv)
    (f19: 'x19 conv)
    (f20: 'x20 conv)
    (ast: ast_value): ('x1, 'x2, 'x3, 'x4, 'x5, 'x6, 'x7, 'x8, 'x9, 'x10, 'x11, 'x12, 'x13, 'x14, 'x15, 'x16, 'x17, 'x18, 'x19, 'x20) field20 =
    {fd1 = f1 ast; fd2 = f2 ast; fd3 = f3 ast; fd4 = f4 ast; fd5 = f5 ast; fd6 = f6 ast; fd7 = f7 ast; fd8 = f8 ast; fd9 = f9 ast; fd10 = f10 ast; fd11 = f11 ast; fd12 = f12 ast; fd13 = f13 ast; fd14 = f14 ast; fd15 = f15 ast; fd16 = f16 ast; fd17 = f17 ast; fd18 = f18 ast; fd19 = f19 ast; fd20 = f20 ast}
  
  module type ADT_conv = sig
    type t 
    type fields

    val to_json : t -> fields
    val from_json : fields -> t

  end  

  let from_json 
    (type t) (type fields) 
    (module Conv: ADT_conv with type t = t and type fields = fields)
    (data: fields) : t =
    Conv.from_json data
 
  let map 
    (fconv: 'a conv) 
    (f: 'a -> 'adt -> 'adt) 
    (r: ast_value * 'adt): ast_value * 'adt =
    let ast, adt = r in
    let fval = fconv ast in
    ast, f fval adt
    
  let data (r: ast_value * 'adt) : 'adt =
    let _, adt = r in
    adt

end

