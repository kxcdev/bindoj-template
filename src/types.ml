type student = {
  admission_year: int ;
  name: string }
let rec student_to_json =
  (let string_to_json (x : string) = (`str x : Kxclib.Json.jv)
   and int_to_json (x : int) = (`num (float_of_int x) : Kxclib.Json.jv) in
   fun { admission_year = x0; name = x1 } ->
     `obj
       [("admission_year", (int_to_json x0)); ("name", (string_to_json x1))] : 
  student -> Kxclib.Json.jv)[@@warning "-39"]
and student_of_json =
  (let string_of_json =
     function | (`str x : Kxclib.Json.jv) -> Some x | _ -> None
   and int_of_json =
     function
     | (`num x : Kxclib.Json.jv) -> Some (int_of_float x)
     | (_ : Kxclib.Json.jv) -> None in
   function
   | `obj param ->
       let (>>=) = Option.bind in
       ((List.assoc_opt "admission_year" param) >>= int_of_json) >>=
         ((fun x0 ->
             ((List.assoc_opt "name" param) >>= string_of_json) >>=
               (fun x1 -> Some { admission_year = x0; name = x1 })))
   | _ -> None : Kxclib.Json.jv -> student option)[@@warning "-39"]
type person =
  | Anonymous 
  | Student of student 
let rec person_to_json =
  (function
   | Anonymous -> `obj [("kind", (`str "Anonymous"))]
   | Student (x0) ->
       `obj [("kind", (`str "Student")); ("arg", (student_to_json x0))] : 
  person -> Kxclib.Json.jv)[@@warning "-39"]
and person_of_json =
  (fun __bindoj_orig ->
     (Kxclib.Jv.pump_field "kind" __bindoj_orig) |>
       (function
        | `obj (("kind", `str "Anonymous")::[]) -> Some Anonymous
        | `obj (("kind", `str "Student")::("arg", x0)::[]) ->
            let (>>=) = Option.bind in
            (student_of_json x0) >>= ((fun x0 -> Some (Student x0)))
        | _ -> None) : Kxclib.Json.jv -> person option)[@@warning "-39"]