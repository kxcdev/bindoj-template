open Bindoj_base.Type_desc

let student : type_decl =
  record_decl "student" [
    record_field "admission_year" (Coretype.mk_prim `int);
    record_field "name" (Coretype.mk_prim `string);
  ]

let person : type_decl =
  variant_decl "person" [
    variant_constructor "Anonymous" `no_param;
    variant_constructor "Student" (`tuple_like [Coretype.mk_ident "student"]);
  ]

let () =
  let open Bindoj_gen.Caml_datatype in
  let open Bindoj_gen.Json_codec in
  let student =
    gen_structure
      ~generators:[gen_json_codec ~self_contained:true]
      student
  in
  let person =
    gen_structure
      ~generators:[gen_json_codec ~self_contained:true]
      person
  in
  Astlib.Pprintast.structure Format.std_formatter (student @ person)
